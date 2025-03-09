{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module GHRB.IO.Cmd
  ( defaultEmergeArgs
  , installedArgs
  , repo
  , defaultPqueryArgs
  , defaultHUArgs
  , runTransparent
  ) where

import           Conduit                            (iterMC, sinkLazy)
import           Control.Monad                      (void)
import           Control.Monad.IO.Class             (liftIO)
import qualified Data.ByteString                    as BS (ByteString, hPut)
import qualified Data.ByteString.Lazy               as BL (ByteString)
import           Data.Conduit                       (ConduitT, (.|))
import           Data.Conduit.Process               (sourceProcessWithStreams)
import           Data.Void                          (Void)
import           Effectful                          (Eff, IOE,
                                                     (:>))
import           Effectful.FileSystem               (FileSystem)
import qualified Effectful.FileSystem.IO            as IO (stderr)
import           GHRB.Core.Types                    (Stderr, Stdout)
import           GHRB.IO.Utils                      (printColor)
import           System.Console.ANSI.Types          (Color (Magenta))
import           System.Exit                        (ExitCode)
import           System.IO                          (Handle, stderr, stdout)
import           System.Process                     (proc)

repo :: String
repo = "haskell"

defaultEmergeArgs :: [String]
defaultEmergeArgs =
  [ "--ignore-default-opts"
  , "--verbose"
  , "--quiet-build"
  , "--deep"
  , "--complete-graph"
  , "--oneshot"
  , "--update"
  , "--color=n" -- Need a ANSI filtering library
  , "--nospinner"
  ]

defaultPqueryArgs :: [String]
defaultPqueryArgs = ["--no-version"]

defaultHUArgs :: [String]
defaultHUArgs =
  [ "--"
  , "--ignore-default-opts"
  , "--verbose"
  , "--quiet-build"
  , "--color=n" -- Need a ANSI filtering library
  , "--nospinner"
  ]

installedArgs :: [String]
installedArgs = ["-I"]

-- | Find the path to the @emerge@ executable or throw an error. Caches the
--   result in the case of a success. Sets @FEATURES="-getbinpkg"@ to avoid
--   it interfering with this utility.
-- Note : runOpaque is just readProcessWithExitCod
-- | Run a command and dump stdout to @stdout@, stderr to @stderr@, also
--   capturing both streams.
runTransparent ::
     (IOE :> es, FileSystem :> es)
  => FilePath -- ^ executable path
  -> [String] -- ^ arguments
       -- | Exit code, stdout, stderr
  -> Eff es (ExitCode, Stdout, Stderr)
runTransparent exe args = do
  void . printColor IO.stderr Magenta $ "Running: " ++ showCmd
  liftIO
    (sourceProcessWithStreams
       (proc exe args) -- { delegate_ctlc = True }
       (pure ())
       (transSink stdout)
       (transSink stderr))
  where
    transSink :: Handle -> ConduitT BS.ByteString Void IO BL.ByteString
    transSink h = iterMC (BS.hPut h) .| sinkLazy
    showCmd :: String
    showCmd = unwords $ exe : map showArg args
    showArg :: String -> String
    showArg arg =
      case words arg of
        []  -> ""
        [w] -> w
        ws  -> show $ unwords ws
