-- | The entry point for the Foxworth application.
module Main where

import Control.Monad.IO.Class (MonadIO)
import Data.Text qualified as T
import Log (
    OutputOptions (..),
    Verbosity (..),
    WithLog,
    doLog,
    logDebug,
    logError,
    logInfo,
    logTrace,
    logWarning,
 )
import Options (
    InterpreterOptions (..),
    parseArgs,
 )
import System.Console.ANSI qualified as Ansi
import System.IO (stderr)

-- | Parse CLI options and, if successful, execute the appropriate command.
main ∷ IO ()
main = do
    isStderrColor ← Ansi.hSupportsANSIColor stderr
    let youngSettings = OutputOptions{useColor = isStderrColor, verbosity = Debug}
    (options, outputOpts) ← doLog youngSettings $ parseArgs isStderrColor
    doLog outputOpts $ runInterpreter options

-- | (TODO) Execute a Foxworth file.
runInterpreter ∷
    (WithLog m, MonadIO m) ⇒
    InterpreterOptions →
    m ()
runInterpreter InterpreterOptions{inputFile, optimizeLevel} = do
    logTrace "zero"
    logDebug "one"
    logInfo "two"
    logWarning "three"
    logError "four"
    logInfo $ "Input file: " <> T.pack (show inputFile)
    logInfo $
        "Optimization level: " <> T.pack (show optimizeLevel)
    pure ()
