{-# LANGUAGE FlexibleContexts #-}

module GHRB.IO
  ( randomBuild
  , terminate
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (get, gets, modify)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as B (pack)
import           Data.Foldable          (for_)
import           Data.Maybe             (fromJust, isJust)
import qualified Data.Text              as T (unpack)
import           Data.Time.Clock.System (SystemTime, getSystemTime)
import           FlatParse.Basic        (Result (OK))
import           GHRB.Core              (MonadGHRB, Package, PackageMap,
                                         Running (Running, Terminated),
                                         addTried, failedResolve,
                                         filePathPackage, generator, getUntried,
                                         hasCompleted, hasDowngraded, hasFailed,
                                         logOutput, package, parseDowngrades,
                                         parsePackageList, prettyPackage,
                                         randomPackage, readProcessWithExitCode,
                                         sizeMap, stderr, stdout, toDate,
                                         untried, getEix, getEmerge, getHU, getInterrupt)
import           System.Exit            (ExitCode (ExitFailure, ExitSuccess))
import Control.Monad.Reader (asks)
import Control.Concurrent.MVar (tryTakeMVar)

tmpLogRoot :: String
tmpLogRoot = "/tmp/random-pkg-"

mergeArgs :: [String]
mergeArgs =
  [ "--ignore-default-opts"
  , "--verbose"
  , "--quiet-build"
  , "--deep"
  , "--complete-graph"
  , "--oneshot"
  ]

-- | Arguments for haskellUpdater
huArgs :: [String]
huArgs = ["--", "--ignore-default-opts", "--quiet-build"]

installedArgs :: [String]
installedArgs =
  ["--in-overlay", "haskell", "--and", "-I", "--and", "--non-masked", "-#"]

installableArgs :: [String]
installableArgs = ["--in-overlay", "haskell", "--and", "--non-masked", "-#"]

notInstalledArgs :: [String]
notInstalledArgs =
  [ "--in-overlay"
  , "haskell"
  , "--and"
  , "-("
  , "-!"
  , "-I"
  , "-)"
  , "--and"
  , "--non-masked"
  , "-#"
  ]

runEix :: MonadGHRB m => [String] -> m (ExitCode, Maybe PackageMap, String)
runEix args = do
  eix <- asks getEix
  (exitCode, packageList, stdErr) <- readProcessWithExitCode eix args ""
  case exitCode of
    ExitSuccess -> pure (exitCode, parsePackageList packageList, stdErr)
    _           -> pure (exitCode, Nothing, stdErr)

runEmerge :: MonadGHRB m => [String] -> Package -> m (ExitCode, String, String)
runEmerge args pkg =
  asks getEmerge >>= \emerge -> readProcessWithExitCode
    emerge
    (mergeArgs ++ args ++ [T.unpack . prettyPackage $ pkg])
    ""

runHaskellUpdater :: MonadGHRB m => m (ExitCode, String, String)
runHaskellUpdater = asks getHU >>= \haskellUpdater -> readProcessWithExitCode haskellUpdater huArgs ""

currentUntried :: MonadGHRB m => m (Either Running PackageMap)
currentUntried = do
  (exitCode, notInstalled, stdErr) <- runEix notInstalledArgs
  case exitCode of
    ExitSuccess ->
      case notInstalled of
        (Just packageMap) -> pure . Right $ packageMap
        Nothing ->
          stderr "eix output parsing failed" >> Left <$> terminate Nothing
    ExitFailure 127 ->
      stderr "Received exit code 127 from eix. Is it installed"
        >> pure (Left Terminated)
    ExitFailure 1 -> Left <$> terminate Nothing
    ExitFailure ef ->
      stderr ("eix exited with unsuccessful code " ++ show ef ++ "\n" ++ stdErr)
        >> Left <$> terminate Nothing

selectFrom :: MonadGHRB m => m (Either Running Package)
selectFrom = do
  toTry <- gets untried
  let sp = sizeMap toTry
  case sp of
    0 -> Left <$> terminate (Just $ B.pack "Pool is empty! Exiting...")
    _ -> do
      stderr $ "Choosing from pool of " ++ show sp ++ " packages..."
      g <- gets generator
      let (pkg, g') = randomPackage g toTry
      modify (\x -> x {generator = g'})
      pure . Right $ pkg

tryInstall :: MonadGHRB m => m Running
tryInstall = do
  pkg <- gets package
  stderr $ "Trying " ++ (T.unpack . prettyPackage $ pkg)
  stderr "Checking for downgrades..."
  (exitCode, output) <- capturePortageOutput pkg
  case exitCode of
    ExitSuccess -> processIfNotDowngrade output
    _           -> resolveFailed output >> pure Running

processIfNotDowngrade :: MonadGHRB m => String -> m Running
processIfNotDowngrade output = do
  downgrade <- checkForDowngrades output
  if downgrade
    then downgrades output >> pure Running
    else install

install :: MonadGHRB m => m Running
install = do
  pkg <- gets package
  (exitCode, _, _) <- runEmerge ["--keep-going=y"] pkg
  if exitCode == ExitSuccess
    then modify (hasCompleted pkg)
    else modify (hasFailed pkg)
  (exitCode', _, _) <- runHaskellUpdater
  if exitCode' /= ExitSuccess
    then terminate . Just . B.pack $ "haskell-updater failed"
    else pure Running

resolveFailed :: MonadGHRB m => String -> m ()
resolveFailed output = do
  time <- liftIO getSystemTime
  pkg <- gets package
  logPortageOutput time "resolve-failed-" pkg output
  stderr
    $ "Failure while resolving with portage: "
        ++ (T.unpack . prettyPackage $ pkg)
  modify (failedResolve pkg)

downgrades :: MonadGHRB m => String -> m ()
downgrades output = do
  time <- liftIO getSystemTime
  pkg <- gets package
  logPortageOutput time "downgrade-" pkg output
  modify (hasDowngraded pkg)

logPortageOutput ::
     MonadGHRB m => SystemTime -> String -> Package -> String -> m ()
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

totalStats :: MonadGHRB m => m (Maybe ByteString)
totalStats = do
  (ec, installed, _) <- runEix installedArgs
  (ec', total, _) <- runEix installableArgs
  if ec == ExitSuccess && ec' == ExitSuccess && isJust installed && isJust total
    then do
      let is = sizeMap . fromJust $ installed
          ts = sizeMap . fromJust $ total
          pc = (100 * is) `div` ts
      pure . Just . B.pack
        $ show is
            ++ " installed out of "
            ++ show ts
            ++ " total, "
            ++ show pc
            ++ "%."
    else pure Nothing

terminate :: MonadGHRB m => Maybe ByteString -> m Running
terminate message = do
  for_ message stdout
  st <- get
  stdout . B.pack $ show st
  totalStats >>= mapM_ stdout
  pure Terminated

capturePortageOutput :: MonadGHRB m => Package -> m (ExitCode, String)
capturePortageOutput pkg = do
  emerge <- asks getEmerge
  stderr (emerge ++ " " ++ unwords mergeArgs ++ " " ++ "--pretend --nospinner")
  (exitCode, stdOut, stdErr) <- runEmerge ["--pretend", "--nospinner"] pkg
  let output = stdOut ++ stdErr
  stderr ("pretend_return: " ++ output)
  pure (exitCode, output)

checkForDowngrades :: MonadGHRB m => String -> m Bool
checkForDowngrades portageOutput = do
  let result = parseDowngrades . B.pack $ portageOutput
  case result of
    OK downgraded _ ->
      if downgraded
        then gets package >>= \pkg ->
               stderr
                 ("Downgrade detected: " ++ (T.unpack . prettyPackage $ pkg))
                 >> pure downgraded
        else stderr "No downgrade detected" >> pure downgraded
    _ -> error "generic parser error"

checkInterrupt :: MonadGHRB m => m Running -> m Running
checkInterrupt f = do
  interrupt <- asks getInterrupt
  interrupted <- liftIO $ tryTakeMVar interrupt
  case interrupted of
    Nothing -> f
    _ -> terminate Nothing

randomBuild :: MonadGHRB m => m Running
randomBuild = do
  stderr
    "\n--------\n\nLooking for a random package from ::haskell that is not installed..."
  u <- currentUntried
  case u of
    Left r -> pure r
    Right untried' -> do
      modify . getUntried $ untried'
      checkInterrupt $ do eitherPkg <- selectFrom
                          case eitherPkg of
                              Left r -> pure r
                              Right pkg -> do
                                  modify (\st -> st {package = pkg})
                                  modify (addTried pkg)
                                  checkInterrupt tryInstall
