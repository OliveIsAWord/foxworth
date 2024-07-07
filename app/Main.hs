-- | The entry point for the Foxworth application.
module Main where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO.Utf8 qualified as T
import Desugar (desugar)
import Lex (tokenize)
import Log (
    OutputOptions (..),
    Verbosity (..),
    WithLog,
    doLog,
    logError,
 )
import Options (
    InterpreterOptions (..),
    parseArgs,
 )
import Parse (parse, prettyProgram)
import System.Console.ANSI qualified as Ansi
import System.Exit (exitFailure)
import System.FilePath (takeFileName)
import System.IO (stderr)
import System.IO.Error qualified as E

-- | Parse CLI options and, if successful, execute the appropriate command.
main ∷ IO ()
main = do
    isStderrColor ← Ansi.hSupportsANSIColor stderr
    let youngSettings = OutputOptions{useColor = isStderrColor, verbosity = Debug}
    (options, outputOpts) ← doLog youngSettings $ parseArgs isStderrColor
    doLog outputOpts $ runInterpreter options

getSource ∷ (MonadIO m) ⇒ FilePath → m (Either Text Text)
getSource path = liftIO $ first showError <$> try (T.readFile path)
  where
    showError e = "Could not read `" <> T.pack path <> "`: " <> showErrorKind e
    showErrorKind e
        -- skipping isAlreadyExistsError, isIllegalOperation
        | E.isDoesNotExistError e = "file does not exist"
        | E.isAlreadyInUseError e = "file already in use"
        | E.isFullError e = "ran out of storage space"
        | E.isEOFError e = "unexpected end of file"
        | E.isPermissionError e = "insufficient privilege"
        | E.isResourceVanishedError e = "resource vanished"
        | otherwise = T.strip . last . T.split (== ':') . T.pack $ show e

-- | (TODO) Execute a Foxworth file.
runInterpreter ∷
    (WithLog m, MonadIO m) ⇒
    InterpreterOptions →
    m ()
runInterpreter InterpreterOptions{..} = do
    source ←
        getSource inputFile >>= \case
            Right s → pure s
            Left e → logError e >> liftIO exitFailure
    let fileName = takeFileName inputFile
    tokens ← case tokenize fileName source of
        Left e → do
            logError e
            liftIO exitFailure
        Right x → pure x
    liftIO . print $ take 5 tokens
    foxProgram ← case parse fileName tokens of
        Left i → do
            -- logError . ("Parse error at " <>) . T.pack . show $ tokens !! i
            logError i
            liftIO exitFailure
        Right x → pure x
    -- liftIO . T.putStrLn . prettyFoxProgram $ foxProgram
    let coreProgram = fmap desugar foxProgram
    liftIO . T.putStrLn $ prettyProgram id coreProgram
