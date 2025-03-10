{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import           Control.Exception.Safe (finally)
import           Control.Monad          (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, runReaderT)
import           Control.Monad.State    (MonadState, evalStateT)
import           Control.Monad.Time     (MonadTime)
import           Data.HashSet           (toList)
import           GHRB.Core              (buildEmptyState)
import           GHRB.Core.Types        (Args, Running (Running), St,
                                         getAllPackages, getPquery, untried)
import           GHRB.IO                (allPackages, currentUntried,
                                         randomBuild, terminate)
import           GHRB.IO.Utils          (getArgs, stderr)
import           List.Shuffle           (shuffleIO)

builder ::
     (MonadIO m, MonadTime m, MonadState St m, MonadReader Args m, MonadTime m)
  => m ()
builder = do
  running <- randomBuild
  when (running == Running) (randomBuild >> builder)

main :: IO ()
main = do
  let initialState = buildEmptyState
  args <- getArgs
  rawAp <- allPackages . getPquery $ args
  case rawAp of
    Nothing ->
      flip runReaderT args
        $ stderr "Could not get list of all available packages"
    Just ap ->
      flip runReaderT (args {getAllPackages = ap}) $ do
        rawNotInstalled <- currentUntried
        case rawNotInstalled of
          Left _ -> stderr "Could not get list of uninstalled packages"
          Right set -> do
            untried' <- liftIO . shuffleIO . toList $ set
            let state = initialState {untried = untried'}
            flip evalStateT state $ finally builder terminate
