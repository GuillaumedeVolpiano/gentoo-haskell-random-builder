{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Repeatedly tries to install a random package from ::haskell (that is not
-- already installed and has at least one unmasked version)
--
-- - Notes successes, errors, packages that try to downgrade, and packages that
--   fail to resolve
-- - Rudimentary logger to files: /tmp/random-pkg-*
-- - Runs `haskell-updater` after every attempt -- Aborts script if it fails
-- - Aborts on on SIGINT (Ctrl+C)
-- Based on hololeap's build-random-haskell-pkgs.bash
module Main where

import           Control.Monad                      (void)
import           Control.Monad.State                (MonadState, state)
import           CoreMain                           (runMain)
import qualified Data.ByteString.Char8              as B (pack)
import           Effectful                          (Eff, IOE, runEff, (:>))
import           Effectful.Console.ByteString       (Console, runConsole)
import qualified Effectful.Console.ByteString       as EC (putStrLn)
import           Effectful.FileSystem               (FileSystem, runFileSystem)
import qualified Effectful.FileSystem.IO            as IO (stderr)
import qualified Effectful.FileSystem.IO.ByteString as EF (hPutStrLn, writeFile)
import           Effectful.Process                  (Process, runProcess)
import qualified Effectful.Process                  as EP (readProcessWithExitCode)
import           Effectful.State.Static.Shared      (State, evalState)
import qualified Effectful.State.Static.Shared      as ES (state)
import           GHRB.Core                          (MonadGHRB,
                                                     St,
                                                     bStderr, 
                                                     logOutput,
                                                     readProcessWithExitCode,
                                                     stdout)

instance ( IOE :> es
         , Console :> es
         , Process :> es
         , FileSystem :> es
         , MonadState St (Eff es)
         ) =>
         MonadGHRB (Eff es) where
  readProcessWithExitCode = EP.readProcessWithExitCode
  stdout = EC.putStrLn
  bStderr = EF.hPutStrLn IO.stderr
  logOutput filepath = EF.writeFile filepath . B.pack

instance (MonadState a (Eff es), State a :> es) => MonadState a (Eff es) where
  state = ES.state

runGHRB ::
     St
  -> Eff ([State St, Process, Console, FileSystem, IOE])
       ()
  -> IO ()
runGHRB initialState =
  void
    . runEff
    . runFileSystem
    . runConsole
    . runProcess
    . evalState initialState

main :: IO ()
main = runMain runGHRB
