{-# LANGUAGE FlexibleContexts #-}

module GHRB.IO
  ( randomBuild
  , terminate
  , allPackages
  , currentUntried
  ) where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, asks)
import           Control.Monad.State        (MonadState, get, gets, modify)
import           Control.Monad.Time         (MonadTime, currentTime)
import qualified Data.ByteString.Char8      as BS (pack)
import qualified Data.ByteString.Lazy       as BL (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL (pack)
import qualified Data.HashSet               as Set (difference, intersection,
                                                    size)
import           Data.List                  (uncons)
import qualified Data.Text                  as T (unpack)
import           Data.Time.Clock            (UTCTime)
import           Distribution.Portage.Types (Package)
import           FlatParse.Basic            (Result (OK))
import           GHRB.Core                  (addTried, failedResolve,
                                             filePathPackage, hasCompleted,
                                             hasDowngraded, hasFailed,
                                             parseDowngrades, parsePackageList,
                                             prettyPackage, toDate,
                                             updateInstalled)
import           GHRB.Core.Types            (Args,
                                             EmergeResult (BuildFailed, EmergeSuccess),
                                             PackageSet,
                                             PrelimEmergeResult (PrelimEmergeSuccess, ResolveFailed, TriedToDowngrade),
                                             Running (Running, Terminated), St,
                                             Stderr, Stdout, getAllPackages,
                                             getEmerge, getHU, getPquery,
                                             installed, package, untried)
import           GHRB.IO.Cmd                (defaultEmergeArgs, defaultHUArgs,
                                             defaultPqueryArgs, installedArgs,
                                             repo, runTransparent)
import           GHRB.IO.Utils              (bStderr, logOutput, stderr, stdout)
import           System.Exit                (ExitCode (ExitFailure, ExitSuccess))
import           System.Process             (readProcessWithExitCode)

tmpLogRoot :: String
tmpLogRoot = "/tmp/random-pkg-"

runPquery ::
     (MonadIO m, MonadReader Args m)
  => [String]
  -> m (ExitCode, Maybe PackageSet, String)
runPquery args = do
  pquery <- asks getPquery
  let args' = args ++ defaultPqueryArgs
  (exitCode, packageList, stdErr) <- liftIO $ readProcessWithExitCode pquery args' ""
  case exitCode of
    ExitSuccess -> pure (exitCode, parsePackageList packageList, stdErr)
    _           -> pure (exitCode, Nothing, stdErr)

allPackages :: MonadIO m => FilePath -> m (Maybe PackageSet)
allPackages pquery = do
  let args = defaultPqueryArgs ++ ["--repo", repo]
  (exitCode, packageList, _) <- liftIO $ readProcessWithExitCode pquery args ""
  case exitCode of
    ExitSuccess -> pure (parsePackageList packageList)
    _           -> pure Nothing

runEmerge ::
     (MonadIO m, MonadReader Args m)
  => [String]
  -> Package
  -> m (ExitCode, String, String)
runEmerge args pkg =
  asks getEmerge >>= \emerge ->
    liftIO $ readProcessWithExitCode
      emerge
      (defaultEmergeArgs ++ args ++ [T.unpack . prettyPackage $ pkg])
      ""

runHaskellUpdater ::
     (MonadIO m, MonadReader Args m) => m (ExitCode, Stdout, Stderr)
runHaskellUpdater =
  asks getHU >>= \haskellUpdater -> runTransparent haskellUpdater defaultHUArgs

currentUntried ::
  (MonadIO m, MonadReader Args m)
  => m (Either Running PackageSet)
currentUntried = do
  ap <- asks getAllPackages
  rawInstalled <- currentInstalled
  case rawInstalled of
    Left _     -> pure rawInstalled
    Right inst -> pure . Right $ Set.difference ap inst

currentInstalled ::
     (MonadReader Args m, MonadIO m)
  => m (Either Running PackageSet)
currentInstalled = do
  (exitCode, inst, stdErr) <- runPquery installedArgs
  case exitCode of
    ExitSuccess ->
      case inst of
        (Just packageSet) -> pure . Right $ packageSet
        Nothing ->
          stderr "pquery output parsing failed" >> pure (Left Terminated)
    ExitFailure 127 ->
      stderr "Received exit code 127 from pquery. Is it installed"
        >> pure (Left Terminated)
    ExitFailure 1 -> pure (Left Terminated)
    ExitFailure ef ->
      stderr
        ("pquery exited with unsuccessful code " ++ show ef ++ "\n" ++ stdErr)
        >> pure (Left Terminated)

tryInstall ::
     ( MonadIO m
     , MonadReader Args m
     , MonadState St m
     , MonadTime m
     )
  => m (PrelimEmergeResult, String)
tryInstall = do
  pkg <- gets package
  stderr $ "Trying " ++ (T.unpack . prettyPackage $ pkg)
  stderr "Checking for downgrades..."
  (exitCode, output) <- capturePortageOutput pkg
  case exitCode of
    ExitSuccess -> processIfNotDowngrade output
    _           -> currentTime >>= \time -> pure (ResolveFailed time, output)

processIfNotDowngrade ::
     ( MonadReader Args m
     , MonadIO m
     , MonadState St m
     , MonadTime m
     )
  => String
  -> m (PrelimEmergeResult, String)
processIfNotDowngrade output = do
  downgrade <- checkForDowngrades output
  if downgrade
    then currentTime >>= \time -> pure (TriedToDowngrade time, output)
    else pure (PrelimEmergeSuccess, output)

install ::
     ( 
     MonadIO m
     , MonadState St m
     , MonadReader Args m
     , MonadTime m
     )
  => m (EmergeResult, Running)
install = do
  pkg <- gets package
  (exitCode, _, _) <- runEmerge ["--keep-going=y"] pkg
  time <- currentTime
  let result =
        if exitCode == ExitSuccess
          then EmergeSuccess time
          else BuildFailed time
  (exitCode', _, _) <- runHaskellUpdater
  if exitCode' /= ExitSuccess
    then pure (result, Terminated)
    else pure (result, Running)

failed ::
     (MonadState St m, MonadReader Args m, MonadIO m)
  => String
  -> PrelimEmergeResult
  -> m ()
failed output result = do
  pkg <- gets package
  let (prefix, time, message, op) =
        case result of
          ResolveFailed t ->
            ( "resolve-failed-"
            , t
            , " failed while resolving with portage"
            , failedResolve)
          TriedToDowngrade t ->
            ("downgrade-", t, " tried to downgrade", hasDowngraded)
          _ -> undefined
  logPortageOutput time prefix pkg output
  stderr $ (T.unpack . prettyPackage $ pkg) ++ message
  modify (op time pkg)

logPortageOutput ::
     (MonadIO m, MonadReader Args m)
  => UTCTime
  -> String
  -> Package
  -> String
  -> m ()
logPortageOutput time pathCircumstances pkg output = do
  let fullPath = tmpLogRoot ++ pathCircumstances ++ filePathPackage pkg
  stderr $ "Saving output to " ++ fullPath
  logOutput
    fullPath
    ((T.unpack . toDate $ time)
       ++ "\n"
       ++ (T.unpack . prettyPackage $ pkg)
       ++ "\n"
       ++ output)

totalStats ::
     (MonadState St m, MonadReader Args m, MonadIO m) => m BL.ByteString
totalStats = do
  inst <- gets installed
  total <- asks getAllPackages
  let is = Set.size . Set.intersection total $ inst
      ts = Set.size total
      pc = (100 * is) `div` ts
  pure . BL.pack
    $ show is
        ++ " installed out of "
        ++ show ts
        ++ " total, "
        ++ show pc
        ++ "%."

terminate ::
     (MonadIO m, MonadReader Args m, MonadState St m)
  => m ()
terminate = do
  st <- get :: (MonadState St m) => m St
  stdout . BL.pack $ show st
  stdout . BL.pack $ "\n"
  totalStats >>= stdout

capturePortageOutput ::
     (MonadIO m, MonadReader Args m)
  => Package
  -> m (ExitCode, String)
capturePortageOutput pkg = do
  emerge <- asks getEmerge
  stderr
    (emerge
       ++ " "
       ++ unwords defaultEmergeArgs
       ++ " "
       ++ "--pretend --nospinner")
  (exitCode, stdOut, stdErr) <- runEmerge ["--pretend", "--nospinner"] pkg
  let output = stdOut ++ stdErr
  stderr ("pretend_return: " ++ output)
  pure (exitCode, output)

checkForDowngrades ::
     (MonadIO m, MonadReader Args m, MonadState St m)
  => String
  -> m Bool
checkForDowngrades portageOutput = do
  let result = parseDowngrades . BS.pack $ portageOutput
  case result of
    OK downgraded _ ->
      if downgraded
        then gets package >>= \pkg ->
               stderr
                 ("Downgrade detected: " ++ (T.unpack . prettyPackage $ pkg))
                 >> pure downgraded
        else stderr "No downgrade detected" >> pure downgraded
    _ -> error "generic parser error"

randomBuild ::
     ( MonadIO m
     , MonadState St m
     , MonadReader Args m
     , MonadTime m
     )
  => m Running
randomBuild = do
  u <- gets untried
  case uncons u of
    Nothing -> pure Terminated
    Just (pkg, ps) -> do
      modify (\st -> st {package = pkg})
      modify (addTried pkg)
      stderr $ show (length ps + 1) ++ " packages left to consider.\n"
      (preliminaryEmergeResult, preliminaryOutput) <- tryInstall
      r <-
        case preliminaryEmergeResult of
          PrelimEmergeSuccess -> do
            (emergeResult, running) <- install
            case emergeResult of
              BuildFailed t   -> modify (hasFailed t pkg)
              EmergeSuccess t -> modify (hasCompleted t pkg)
            pure running
          _ -> failed preliminaryOutput preliminaryEmergeResult >> pure Running
      if r == Running
        then do
          rawInstalled <- currentInstalled
          case rawInstalled of
            Left r' -> pure r'
            Right inst ->
              currentTime >>= \time ->
                modify (updateInstalled time ps inst)
                  >> totalStats
                  >>= bStderr
                  >> pure r
        else pure r
