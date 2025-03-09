{-# LANGUAGE LambdaCase #-}

module CoreMain
  ( runMain
  ) where

import           Control.Applicative    (optional, (<**>), (<|>))
import           Control.Monad          (when)
import           Control.Monad.IO.Class (liftIO)
import           GHRB.Core              (buildEmptyState)
import           GHRB.Core.Types        (Args (Args), MonadGHRB,
                                         Output (DevNull, OutFile, Std),
                                         Running (Running), St)
import           GHRB.IO                (randomBuild)
import           Options.Applicative    (Parser, execParser, flag', fullDesc,
                                         help, helper, info, long, metavar,
                                         progDesc, short, strOption, value)
import           System.Random          (newStdGen)

args :: Parser Args
args =
  Args
    <$> strOption
          (long "eix"
             <> short 'e'
             <> metavar "Filepath"
             <> value "/usr/bin/eix"
             <> help "Path to the eix binary")
    <*> strOption
          (long "emerge"
             <> short 'm'
             <> metavar "Filepath"
             <> value "/usr/bin/emerge"
             <> help "Path to the emerge binary")
    <*> strOption
          (long "haskell-updater"
             <> short 'u'
             <> metavar "Filepath"
             <> value "/usr/sbin/haskell-updater"
             <> help "Path to the haskell-updater binary")
    <*> ((\case
            Nothing -> Std
            Just a -> a)
           <$> optional (outputFile <|> stdOut))
    <*> ((\case
            Nothing -> Std
            Just a -> a)
           <$> optional (logFile <|> stdErr <|> quiet))

outputFile :: Parser Output
outputFile =
  OutFile
    <$> strOption
          (long "out"
             <> short 'o'
             <> metavar "Filepath"
             <> help "File to output to")

stdOut :: Parser Output
stdOut = flag' Std (long "stdout" <> short 's' <> help "Output to stdout")

logFile :: Parser Output
logFile =
  OutFile
    <$> strOption
          (long "log"
             <> short 'l'
             <> metavar "Filepath"
             <> help "File to log the process to")

stdErr :: Parser Output
stdErr =
  flag' Std (long "stderr" <> short 'e' <> help "Output the log to stderr")

quiet :: Parser Output
quiet = flag' DevNull (long "quiet" <> short 'q' <> help "Be less verbose")

builder :: MonadGHRB m => m ()
builder = do
  running <- randomBuild
  when (running == Running) (randomBuild >> builder)

runMain :: (MonadGHRB m) => (St -> Args -> m () -> IO ()) -> IO ()
runMain runGHRB = do
  initialState <- buildEmptyState <$> liftIO newStdGen
  args' <-
    execParser . info (args <**> helper)
      $ (fullDesc
           <> progDesc
                "A utility to repeatedly randomly build haskell packages from ::haskell")
  runGHRB initialState args' builder
