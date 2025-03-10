{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module GHRB.IO.Utils
  ( printColor
  , bStderr
  , stderr
  , stdout
  , logOutput
  , getArgs
  ) where

import           Control.Applicative        ((<**>))
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, asks)
import qualified Data.ByteString.Char8      as BS (hPutStrLn, pack)
import qualified Data.ByteString.Lazy       as BL (ByteString, appendFile,
                                                   writeFile)
import qualified Data.ByteString.Lazy.Char8 as BL (hPutStrLn, pack)
import           GHRB.Core.Types            (Args, EmergePath,
                                             HaskellUpdaterPath,
                                             Output (DevNull, OutFile, Std),
                                             PqueryPath, args, getEmerge,
                                             getErrMode, getHU, getOutputMode,
                                             getPquery)
import           Options.Applicative        (execParser, fullDesc, helper, info,
                                             progDesc)
import           System.Console.ANSI        (setSGR)
import           System.Console.ANSI.Types  (Color, ColorIntensity (Dull),
                                             ConsoleLayer (Foreground),
                                             SGR (Reset, SetColor))
import           System.Directory           (findExecutable)
import           System.Environment         (setEnv)
import           System.Exit                (die)
import qualified System.IO                  as IO (stderr, stdout)
import           System.IO                  (Handle)

printColor :: MonadIO m => Handle -> Color -> String -> m ()
printColor h color message = do
  liftIO $ setSGR [SetColor Foreground Dull color]
  liftIO . BS.hPutStrLn h . BS.pack $ message
  liftIO $ setSGR [Reset]
  liftIO . BS.hPutStrLn h . BS.pack $ ""

stdout :: (MonadIO m, MonadReader Args m) => BL.ByteString -> m ()
stdout message = do
  outmode <- asks getOutputMode
  case outmode of
    Std        -> liftIO $ BL.hPutStrLn IO.stdout message
    OutFile fp -> liftIO $ BL.appendFile fp message
    DevNull    -> pure ()

logOutput :: MonadIO m => FilePath -> String -> m ()
logOutput filepath message = liftIO $ BL.writeFile filepath . BL.pack $ message

stderr :: (MonadIO m, MonadReader Args m) => String -> m ()
stderr = bStderr . BL.pack

bStderr :: (MonadIO m, MonadReader Args m) => BL.ByteString -> m ()
bStderr message = do
  errmode <- asks getErrMode
  case errmode of
    DevNull      -> pure ()
    Std          -> liftIO $ BL.hPutStrLn IO.stderr message
    (OutFile fp) -> liftIO $ BL.appendFile fp message

getArgs :: MonadIO m => m Args
getArgs = do
  args' <-
    liftIO
      $ execParser . info (args <**> helper)
      $ (fullDesc
           <> progDesc
                "A utility to repeatedly randomly build haskell packages from ::haskell")
  pquery <-
    if null (getPquery args')
      then pqueryPath
      else pure . getPquery $ args'
  emerge <-
    if null (getEmerge args')
      then emergePath
      else pure . getEmerge $ args'
  hu <-
    if null (getHU args')
      then haskellUpdaterPath
      else pure . getHU $ args'
  pure args' {getPquery = pquery, getEmerge = emerge, getHU = hu}

emergePath :: MonadIO m => m EmergePath
emergePath =
  liftIO (findExecutable "emerge") >>= \case
    Just p -> do
      liftIO $ setEnv "FEATURES" "-getbinpkg"
      pure p
    Nothing ->
      liftIO
        $ die
            "Could not find emerge executable. Install sys-apps/portage first."

-- | Find the path to the @pquery@ executable or throw an error. Caches the
--   result in the case of a success.
pqueryPath :: MonadIO m => m PqueryPath
pqueryPath =
  liftIO (findExecutable "pquery") >>= \case
    Just p -> pure p
    Nothing ->
      liftIO
        $ die
            "Could not find pquery executable. Install sys-apps/pkgcore first."

-- | Find the path to the @haskell-updater@ executable or throw an error.
--   Caches the result in the case of a success. Sets
--   @FEATURES="-getbinpkg"@ to avoid it interfering with this utility.
haskellUpdaterPath :: MonadIO m => m HaskellUpdaterPath
haskellUpdaterPath =
  liftIO (findExecutable "haskell-updater") >>= \case
    Just p -> do
      liftIO $ setEnv "FEATURES" "-getbinpkg"
      pure p
    Nothing ->
      liftIO
        $ die
            "Could not find haskell-updater executable. \
           \Install app-admin/haskell-updater first."
