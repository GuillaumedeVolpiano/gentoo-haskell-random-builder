module CoreMain
  ( runMain
  ) where

import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                                          tryTakeMVar)
import           Control.Monad           (void)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Reader    (asks)
import qualified Data.ByteString.Char8   as B (pack)
import           Data.Maybe              (isNothing)
import           GHRB.Core               (Args (Args), MonadGHRB,
                                          Running (Running), St,
                                          buildEmptyState, getInterrupt)
import           GHRB.IO                 (randomBuild, terminate)
import           Options.Applicative     (Parser, execParser, fullDesc, help,
                                          info, long, metavar, progDesc, short,
                                          strOption, value, helper, (<**>))
import           System.IO               (BufferMode (NoBuffering),
                                          hSetBuffering, stderr, stdout)
import           System.Posix.Signals    (Handler (Catch), installHandler,
                                          sigINT)
import           System.Random           (newStdGen)

args :: MVar () -> Parser Args
args interrupt =
  Args interrupt
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

builder :: MonadGHRB m => m ()
builder = do
  interrupt <- asks getInterrupt
  interrupted <- liftIO $ tryTakeMVar interrupt
  running <- randomBuild
  if isNothing interrupted && running == Running
    then randomBuild >> builder
    else void (terminate (Just . B.pack $ "interrupted"))

runMain :: (MonadGHRB m) => (St -> Args -> m () -> IO ()) -> IO ()
runMain runGHRB = do
  interrupt <- newEmptyMVar
  initialState <- buildEmptyState <$> liftIO newStdGen
  args' <- execParser . info (args interrupt <**> helper) $ (fullDesc <> progDesc "A utility to repeatedly randomly build haskell packages from ::haskell")
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  void . installHandler sigINT (Catch $ putMVar interrupt ()) $ Nothing
  runGHRB initialState args' builder
