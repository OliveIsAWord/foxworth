-- | CLI parsing.
module Options (
    ColorOption (..),
    InterpreterOptions (..),
    parseArgs,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text qualified as T
import Log (OutputOptions (..), Verbosity (..), WithLog, logError)
import Options.Applicative qualified as P
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import Text.Read qualified as TRead
import Utils (getAll, readPrecImpl)

-- | Command line option for whether to output logs in color.
data ColorOption
    = -- | Heuristically determine if the handle supports displaying ANSI color.
      Auto
    | -- | Enable color output.
      Always
    | -- | Disable color output.
      Never
    deriving (Eq, Enum, Bounded)

instance Show ColorOption where
    show = \case
        Auto → "auto"
        Always → "always"
        Never → "never"

instance TRead.Read ColorOption where
    readPrec = readPrecImpl

colorOption ∷ Bool → ColorOption → Bool
colorOption x Auto = x
colorOption _ Always = True
colorOption _ Never = False

-- | Parameters passed to the interpreter.
data InterpreterOptions = InterpreterOptions
    { inputFile ∷ FilePath
    , optimizeLevel ∷ Int
    }
    deriving (Show)

-- | CLI parser for interpreter options.
interpreterP ∷ P.Parser InterpreterOptions
interpreterP =
    InterpreterOptions
        <$> P.strArgument
            (P.metavar "FILE" <> P.help "[TODO] Input file to execute")
        <*> P.option
            P.auto
            ( P.short 'O'
                <> P.metavar "N"
                <> P.value 0
                <> P.showDefault
                <> P.help "[TODO] Optimization level"
            )

-- | CLI parser for log output options.
outputP ∷ Bool → P.Parser OutputOptions
outputP colorGuess =
    OutputOptions
        <$> fmap
            (colorOption colorGuess)
            ( P.option
                P.auto
                ( P.long "color"
                    <> P.metavar "WHEN"
                    <> P.value Auto
                    <> P.showDefault
                    <> P.help
                        ("Output with ANSI coloring " <> show (getAll @ColorOption))
                )
            )
        <*> P.option
            P.auto
            ( P.short 'v'
                <> P.long "verbosity"
                <> P.metavar "LEVEL"
                <> P.value Info
                <> P.showDefault
                <> P.help
                    ("Output at verbosity level " <> show (getAll @Verbosity))
            )

-- | Parse CLI options, gracefully handling success and errors.
parseArgs ∷
    (WithLog m, MonadIO m) ⇒
    Bool →
    m (InterpreterOptions, OutputOptions)
parseArgs colorGuess = do
    args ← (\case [] → ["--help"]; args → args) <$> liftIO getArgs
    -- args <- toList . fromMaybe ["--help"] . nonEmpty <$> liftIO getArgs
    -- args <- maybe ["--help"] toList . nonEmpty <$> liftIO getArgs
    handleParseResult $ P.execParserPure myPrefs opts args
  where
    myPrefs = P.prefs $ mconcat [P.disambiguate]
    opts =
        P.info
            (liftA2 (,) interpreterP (outputP colorGuess) P.<**> P.helper)
            ( P.fullDesc
                <> P.progDesc "Execute a Foxworth program"
                <> P.header "Foxworth - A System Fω evaluator"
            )

-- | Handles the CLI parser output, erroring and exiting if appropriate.
handleParseResult ∷ (WithLog m, MonadIO m) ⇒ P.ParserResult a → m a
handleParseResult (P.Success a) = pure a
handleParseResult (P.Failure failure) = do
    progn ← liftIO getProgName
    let (msg, exit) = P.renderFailure failure progn
    case exit of
        ExitSuccess → liftIO $ putStrLn msg
        _ → logError $ T.pack msg
    liftIO $ exitWith exit
handleParseResult (P.CompletionInvoked compl) = liftIO $ do
    progn ← getProgName
    msg ← P.execCompletion compl progn
    putStr msg
    exitSuccess
