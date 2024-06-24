-- | Error and diagnostic reporting.
module Log (
    OutputOptions (..),
    Verbosity (..),
    WithLog,
    doLog,
    logTrace,
    logDebug,
    logInfo,
    logWarning,
    logError,
) where

import Colog.Core.Action (LogAction (..), cfilter, hoistLogAction)
import Colog.Core.Class (HasLog, getLogAction)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, MonadTrans, asks, lift, runReaderT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time.LocalTime (
    LocalTime (..),
    ZonedTime (..),
    getZonedTime,
 )
import GHC.Stack (
    CallStack,
    HasCallStack,
    SrcLoc (..),
    callStack,
    getCallStack,
    withFrozenCallStack,
 )
import System.Console.ANSI qualified as Ansi
import System.IO (stderr)
import Text.Read qualified as TRead
import Utils (getAll, readPrecImpl)
import Prelude hiding (log)

{- Note [Logging Framework]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We use the co-log library for easy monadic logging. This probably has some poor effects for the code, such as cluttering code with monads, and artificially serializing much computation by the nature of monads and monadic @do@.
-}

-- | Text to be logged with an associated 'Verbosity' and source location.
data Message = Msg
    { msgSeverity ∷ Verbosity
    , msgStack ∷ CallStack
    , msgText ∷ T.Text
    }

{- | The level of importance for logs. The user can specify a threshold for the minimum level that ought to be printed.

Note: 'Off' should not be used by logs as a verbosity, only as a threshold. This means many functions operating on a 'Verbosity' are partial. This is unfortunate and should be fixed with better types!
-}
data Verbosity
    = -- | Disable log output.
      Off
    | -- | Faults that prevent correct execution.
      Error
    | -- | Potentially hazardous situations. The application likely will not behave as the user expected.
      Warning
    | -- | Useful information. The default threshold.
      Info
    | -- | Ancillary information. May be useful for debugging application behavior.
      Debug
    | -- | Detailed internal information. Generally only useful for Foxworth maintainers.
      Trace
    deriving (Eq, Ord, Enum, Bounded)

-- | Maps verbosities to what colors they should be displayed as if color output is enabled. Errors on 'Off'.
verbosityColor ∷ Verbosity → Ansi.Color
verbosityColor = \case
    Off → undefined
    Error → Ansi.Red
    Warning → Ansi.Yellow
    Info → Ansi.White
    Debug → Ansi.Green
    Trace → Ansi.Blue

instance Show Verbosity where
    show = \case
        Trace → "trace"
        Debug → "debug"
        Info → "info"
        Warning → "warning"
        Error → "error"
        Off → "off"

instance TRead.Read Verbosity where
    readPrec = readPrecImpl

-- | Log a message at a given verbosity.
logTrace, logDebug, logInfo, logWarning, logError ∷ (WithLog m) ⇒ T.Text → m ()
logTrace = log Trace
logDebug = log Debug
logInfo = log Info
logWarning = log Warning
logError = log Error

-- | Logs the message with given severity @sev@.
log ∷ (WithLog m) ⇒ Verbosity → T.Text → m ()
log msgSeverity msgText =
    withFrozenCallStack (logMsg Msg{msgStack = callStack, ..})

logMsg ∷ (WithLog m) ⇒ Message → m ()
logMsg msg = do
    LogAction log ← asks getLogAction
    log msg

newtype LoggerT msg m a = LoggerT
    { runLoggerT ∷ ReaderT (LogAction (LoggerT msg m) msg) m a
    }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadFail
        , MonadReader (LogAction (LoggerT msg m) msg)
        , MonadUnliftIO
        )

usingLoggerT ∷ (Monad m) ⇒ LogAction m msg → LoggerT msg m a → m a
usingLoggerT action lt = runReaderT lt.runLoggerT $ liftLogAction action

liftLogAction ∷
    (Monad m, MonadTrans t) ⇒ LogAction m msg → LogAction (t m) msg
liftLogAction = hoistLogAction lift

instance MonadTrans (LoggerT msg) where
    lift ∷ (Monad m) ⇒ m a → LoggerT msg m a
    lift = LoggerT . lift

-- | Convenient type synonym for use by any function that logs.
type WithLog m = WithLog' (LogAction m Message) Message m

type WithLog' env msg m = (MonadReader env m, HasLog env msg m, HasCallStack)

-- | A map of log output settings.
data OutputOptions = OutputOptions
    { useColor ∷ Bool
    , verbosity ∷ Verbosity
    }

{- Note [Custom error formatting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Our logging can provide a litany of displayable data (message severity/verbosity, timestamp, and stacktrace), with the potential for much more such as compilation stage or thread ID. There may be extended granularity in how each of these options are displayed; a full timestamp may include precise date, time to the nanosecond, and time zone. Displaying all of these data at full specificity will only serve to disastrously clutter, but occasionally one or more will be helpful.

In the face of this range of choices, there are at least a few feasible options:

1. Enforce a single style, most likely one which displays little to no ancillary information. This
   is what most software does, including this program at present.

2. Provide these choices through an array of CLI options. We already have the structure to implement
   this, via optparse-applicative. This would require a balance between customization and brevity.
   Too many options would make the length of invocations and help text unwieldy, as well as increase
   technical cost.

3. Allow the user to pass in a string template that we can format, e.g.
       "[{severity}] [{localTime:%H:%M:%S}] {msg}"
   This is the most powerful and flexible option, but also the one with the greatest complexity. A
   user who simply wants logs to include a timestamp will have to find and learn the string template
   language and the particular identifiers we use. It also requires us to expose additional surface
   area that requires stress-testing against arbitrary user data. The templating functionality would
   require either our own dedicated implementation (out of scope currently) or use an existing
   library such as text-format-heavy (tying us to that library's syntax and semantics). On the other
   hand, it would also allow our ad-hoc `doLog` implementation to be rewritten as a single string
   template. This is the option I (Olive Welch) am most leaning towards.
-}

-- | Output logs to stderr according to the provided settings. Errors on a log with verbosity 'Off'.
doLog ∷ OutputOptions → LoggerT Message IO a → IO a
doLog OutputOptions{..} =
    usingLoggerT
        . cfilter (\Msg{..} → msgSeverity <= verbosity)
        $ LogAction logMessage
  where
    logMessage ∷ Message → IO ()
    logMessage msg = do
        -- taking the time here may be incorrect; should we do this in 'log'?
        time ← getZonedTime
        T.hPutStrLn stderr $ fmtMessage time msg
    fmtMessage time msg = T.concat $ prepends time msg <> [msg.msgText]
    prepends time Msg{..} =
        case msgSeverity of
            Info → []
            Off → error "log message of severity `Off`"
            s → [showVerbosity s, showTime time, showSourceLoc msgStack]
    showVerbosity s =
        color (verbosityColor s)
            . square
            . T.justifyRight maxVerbosityLen ' '
            . T.pack
            $ show s
    maxVerbosityLen
        | padSeverity =
            maximum . map (length . show) $ getAll @Verbosity
        | otherwise = 0 ∷ Int
    -- [YYYY-MM-DD HH:MM:SS.SSS TIMEZONE]
    showTime ZonedTime{zonedTimeToLocalTime = LocalTime{..}, ..} =
        if useTimestamp
            then
                square . T.intercalate " " $
                    map
                        T.pack
                        [ show localDay
                        , take timestampLen (show localTimeOfDay)
                        , show zonedTimeZone
                        ]
            else ""
    timestampLen =
        8
            + ( if subSecondPrecision <= 0
                    then 0
                    else subSecondPrecision + 1
              )
    showSourceLoc cs = if useLoc then square $ showCallStack cs else ""
    -- [stackDepth^Module.function:line]
    showCallStack cs =
        T.pack (show . length $ getCallStack cs) <> "^" <> case getCallStack cs of
            [] → "<unknown loc>"
            [(name, loc)] → showLoc name loc
            (_, loc) : (callerName, _) : _ → showLoc callerName loc
    showLoc name src =
        T.pack src.srcLocModule
            <> "."
            <> T.pack name
            <> ":"
            <> T.pack (show src.srcLocStartLine)
    color c txt =
        if useColor
            then
                T.pack
                    ( Ansi.setSGRCode [Ansi.SetColor Ansi.Foreground Ansi.Vivid c]
                    )
                    <> txt
                    <> T.pack (Ansi.setSGRCode [Ansi.Reset])
            else txt
    square t = "[" <> t <> "] "
    -- These options will theoretically be user adjustable in the future.
    -- See Note [Custom error formatting]
    useLoc = False
    useTimestamp = False
    padSeverity = False
    subSecondPrecision = 3
