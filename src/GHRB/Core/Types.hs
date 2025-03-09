{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module GHRB.Core.Types
  ( AttemptSets
  , Stdout
  , Stderr
  , PqueryPath
  , EmergePath
  , HaskellUpdaterPath
  , --Console arguments
    Args(Args)
  , getPquery
  , getEmerge
  , getHU
  , getOutputMode
  , getErrMode
  , getAllPackages
  , args
  , -- The process state
    St(St)
  , completed
  , failed
  , downgrade
  , installed
  , tried
  , unresolved
  , untried
  , package
  , -- OutputMode
    Output(Std, DevNull, OutFile)
  , PackageSet
  , -- Running state
    Running(Running, Terminated)
  , -- Hashable time
    HashableUTCTime
  , fromHashable
  , toHashable
  , PrelimEmergeResult(PrelimEmergeSuccess, ResolveFailed,
                   TriedToDowngrade)
  , EmergeResult(BuildFailed, EmergeSuccess)
  ) where

import           Control.Applicative        (optional, (<|>))
import qualified Data.ByteString.Lazy       as BL (ByteString)
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as Set (map, size, toList)
import           Distribution.Portage.Types (Package, getCategory, getPkgName,
                                             unwrapCategory, unwrapPkgName)
import           Options.Applicative        (Parser, flag', help, long, metavar,
                                             short, strOption, value)

import           Data.Hashable              (Hashable, hashWithSalt)
import           Data.Time.Clock            (UTCTime)
import           Data.Time.Clock.POSIX      (utcTimeToPOSIXSeconds)

type Set = HashSet

type EmergePath = FilePath

type PqueryPath = FilePath

type HaskellUpdaterPath = FilePath

data PrelimEmergeResult
  = ResolveFailed UTCTime
  | TriedToDowngrade UTCTime
  | PrelimEmergeSuccess
  deriving (Show, Eq, Ord)

data EmergeResult
  = BuildFailed UTCTime
  | EmergeSuccess UTCTime
  deriving (Show, Eq, Ord)

type ResolveFailedAttempts = Set (HashableUTCTime, Package)

type TriedToDowngradeAttempts = Set (HashableUTCTime, Package)

type BuildFailedAttempts = Set (HashableUTCTime, Package)

type EmergeSuccessAttempts = Set (HashableUTCTime, Package)

type AttemptSets
  = ( ResolveFailedAttempts
    , TriedToDowngradeAttempts
    , BuildFailedAttempts
    , EmergeSuccessAttempts)

type Stdout = BL.ByteString

type Stderr = BL.ByteString

-- | A monad class to output messages. Minimum complete definition stdout,
-- readProcessWithExitCode, bStdErr || stderr, logOutput
data Args = Args
  { getPquery      :: String
  , getEmerge      :: String
  , getHU          :: String
  , getOutputMode  :: Output
  , getErrMode     :: Output
  , getAllPackages :: PackageSet
  }

type PackageSet = Set Package

-- | Our current build state
data St = St
  { completed  :: EmergeSuccessAttempts
  , failed     :: BuildFailedAttempts
  , downgrade  :: TriedToDowngradeAttempts
  , unresolved :: ResolveFailedAttempts
  , tried      :: PackageSet
  , installed :: PackageSet
  , untried    :: [Package]
  , package    :: Package
  } deriving (Eq)

instance Show St where
  show st =
    "\n--------\n\nResults:\n\nCompleted:\n"
      ++ prettyPrintSet (Set.map snd $ completed st)
      ++ "\nFailed:\n"
      ++ prettyPrintSet (Set.map snd $ failed st)
      ++ "\nThese packages tried to downgrade:\n"
      ++ prettyPrintSet (Set.map snd $ downgrade st)
      ++ "\nThese packages failed to resolve:\n"
      ++ prettyPrintSet (Set.map snd $ unresolved st)
      ++ "\n\nStatistics:\n\nsuccess: "
      ++ show sc
      ++ "\nfailures: "
      ++ show sf
      ++ "\ndowngrades: "
      ++ show sd
      ++ "\nunresolved: "
      ++ show sur
      ++ "\nuntried: "
      ++ show sun
      ++ "\nsuccessrate: "
      ++ show sr
      ++ "%"
    where
      sc = Set.size . completed $ st
      sf = Set.size . failed $ st
      sd = Set.size . downgrade $ st
      sur = Set.size . unresolved $ st
      sun = length . untried $ st
      sr =
        if sc + sf + sd + sur == 0
          then 0
          else (sc * 100) `div` (sc + sf + sd + sur)

newtype HashableUTCTime = HashableUTCTime
  { fromHashable :: UTCTime
  } deriving (Eq)

instance Hashable HashableUTCTime where
  hashWithSalt salt =
    hashWithSalt salt . toRational . utcTimeToPOSIXSeconds . fromHashable

toHashable :: UTCTime -> HashableUTCTime
toHashable = HashableUTCTime

prettyPrintSet :: PackageSet -> String
prettyPrintSet = unlines . map prettyPrintPackage . Set.toList

prettyPrintPackage :: Package -> String
prettyPrintPackage p =
  (unwrapCategory . getCategory $ p) ++ "/" ++ (unwrapPkgName . getPkgName $ p)

data Output
  = Std
  | DevNull
  | OutFile String

-- | An ADT to report whether the program should have terminated
data Running
  = Terminated
  | Running
  deriving (Eq)

args :: Parser Args
args =
  Args
    <$> strOption
          (long "pquery"
             <> short 'p'
             <> metavar "Filepath"
             <> value ""
             <> help "Path to the eix binary")
    <*> strOption
          (long "emerge"
             <> short 'e'
             <> metavar "Filepath"
             <> value ""
             <> help "Path to the emerge binary")
    <*> strOption
          (long "haskell-updater"
             <> short 'u'
             <> metavar "Filepath"
             <> value ""
             <> help "Path to the haskell-updater binary")
    <*> ((\case
            Nothing -> Std
            Just a -> a)
           <$> optional (outputFile <|> stdOut))
    <*> ((\case
            Nothing -> Std
            Just a -> a)
           <$> optional (logFile <|> stdErr <|> quiet))
    <*> pure mempty

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
