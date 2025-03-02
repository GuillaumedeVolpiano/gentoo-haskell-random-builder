module CoreMain
  ( runMain
  ) where

import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                                          tryTakeMVar)
import           Control.Monad           (void)
import Control.Monad.IO.Class (liftIO)
import           Control.Monad.State     (MonadState, state)
import qualified Data.ByteString.Char8   as B (pack)
import           Data.Maybe              (isNothing)
import           GHRB.Core               (MonadGHRB, Running (Running), St, buildEmptyState)
import           GHRB.IO                 (randomBuild, terminate)
import           System.IO               (BufferMode (NoBuffering),
                                          hSetBuffering, stdout, stderr)
import           System.Posix.Signals    (Handler (Catch), addSignal,
                                          emptySignalSet, installHandler,
                                          sigINT, sigKILL, sigTERM)
import           System.Random           (newStdGen)

builder :: MonadGHRB m => MVar () -> m ()
builder interrupt = do
  interrupted <- liftIO $ tryTakeMVar interrupt
  running <- randomBuild
  if isNothing interrupted && running == Running
    then randomBuild >> builder interrupt
    else void (terminate (Just . B.pack $ "interrupted"))

runMain :: (MonadGHRB m) => (St -> m () -> IO ()) -> IO ()
runMain runGHRB = do
  interrupt <- newEmptyMVar
  initialState <- buildEmptyState <$> liftIO newStdGen
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  void . installHandler sigINT (Catch $ putMVar interrupt ())
          $ Just . addSignal sigKILL . addSignal sigTERM
          $ emptySignalSet
  runGHRB initialState $ builder interrupt
