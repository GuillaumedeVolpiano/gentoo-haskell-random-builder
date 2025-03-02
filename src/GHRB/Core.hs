{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module GHRB.Core
  ( -- The process state
    St
  , buildEmptyState
  , completed
  , failed
  , downgrade
  , tried
  , unresolved
  , untried
  , package
  , generator
  , -- State loggers
    failedResolve
  , hasDowngraded
  , hasCompleted
  , hasFailed
  , addTried
  , -- Running state
    Running(Running, Terminated)
  , -- Package Map
    PackageMap
  , sizeMap
  , -- Package parser
    parsePackageList
  , parseDowngrades
  , -- Package type
    Package
  , -- prettyPrinters
    toDate
  , prettyPackage
  , filePathPackage
  , -- randomPackage generator
    randomPackage
  , -- untried packages checker
    getUntried
  , -- the GHRB Monad class
    MonadGHRB
  , stdout
  , readProcessWithExitCode
  , stderr
  , bStderr
  , logOutput
  ) where

import           Control.Applicative     (many, optional, (<|>))
import           Control.Monad           (void)
import           Control.Monad.IO.Class  (MonadIO)
import           Control.Monad.State     (MonadState)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as B (pack, unpack)
import           Data.HashMap.Strict     (HashMap, alter, elems, keys, (!))
import qualified Data.HashMap.Strict     as Map (differenceWith, size, toList)
import           Data.HashSet            (HashSet)
import qualified Data.HashSet            as Set (difference, foldr, insert,
                                                 null, singleton, size, toList,
                                                 unions)
import           Data.Text               (Text)
import qualified Data.Text               as T (append, cons, pack, unpack)
import           Data.Time.Clock.System  (SystemTime, systemToUTCTime)
import           Data.Time.Format        (defaultTimeLocale, formatTime)
import           Data.Void               (Void)
import           FlatParse.Basic         (Parser, Result (OK), char, eof,
                                          runParser, satisfy, string)
import           System.Exit             (ExitCode)
import           System.Random           (StdGen, randomR)

-- | A monad class to output messages. Minimum complete definition stdout,
-- readProcessWithExitCode, bStdErr || stderr, logOutput
class (Monad m, MonadIO m, MonadState St m) =>
      MonadGHRB m
  where
  stdout :: ByteString -> m ()
  readProcessWithExitCode ::
       FilePath -> [String] -> String -> m (ExitCode, String, String)
  stderr :: String -> m ()
  stderr = bStderr . B.pack
  bStderr :: ByteString -> m ()
  bStderr = stderr . B.unpack
  logOutput :: FilePath -> String -> m ()

type Package = (Text, Text)

type PackageMap = HashMap Text (HashSet Text)

-- | An ADT to report whether the program should have terminated
data Running
  = Terminated
  | Running
  deriving (Eq)

-- | Our current build state
data St = St
  { completed  :: PackageMap
  , failed     :: PackageMap
  , downgrade  :: PackageMap
  , unresolved :: PackageMap
  , tried      :: PackageMap
  , untried    :: PackageMap
  , package    :: Package
  , generator  :: StdGen
  } deriving (Eq)

instance Show St where
  show st =
    "\n--------\n\nResults:\n\nCompleted:\n"
      ++ prettyPrintMap (completed st)
      ++ "\nFailed:\n"
      ++ prettyPrintMap (failed st)
      ++ "\nThese packages tried to downgrade:\n"
      ++ prettyPrintMap (downgrade st)
      ++ "\nThese packages failed to resolve:\n"
      ++ prettyPrintMap (unresolved st)
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
      sc = Set.size . Set.unions . elems . completed $ st
      sf = Set.size . Set.unions . elems . failed $ st
      sd = Set.size . Set.unions . elems . downgrade $ st
      sur = Set.size . Set.unions . elems . unresolved $ st
      sun = Set.size . Set.unions . elems . untried $ st
      sr =
        if sc + sf + sd + sur == 0
          then 0
          else (sc * 100) `div` (sc + sf + sd + sur)

buildEmptyState :: StdGen -> St
buildEmptyState = St mempty mempty mempty mempty mempty mempty undefined

prettyPrintMap :: PackageMap -> String
prettyPrintMap = unlines . map (uncurry prettyPrintCategory) . Map.toList

prettyPrintCategory :: Text -> HashSet Text -> String
prettyPrintCategory category = unlines . Set.foldr prettyName []
  where
    prettyName name = (T.unpack (T.append category . T.cons '/' $ name) :)

parsePackageList :: String -> Maybe PackageMap
parsePackageList packageList =
  case runParser parsePackages . B.pack $ packageList of
    OK packageSet _ -> Just packageSet
    _               -> Nothing

parsePackages :: Parser Void PackageMap
parsePackages = parsePackage <|> (eof >> return mempty)

parsePackage :: Parser Void PackageMap
parsePackage = do
  category <- T.pack <$> many (satisfy (/= '/'))
  $(char '/')
  name <- T.pack <$> many (satisfy (/= '\n'))
  void . optional $ $(char '\n')
  alter (categoryAlter name) category <$> parsePackages

parseDowngrades :: ByteString -> Result Void Bool
parseDowngrades = runParser parseDowngradeByLine

parseDowngradeByLine :: Parser Void Bool
parseDowngradeByLine =
  ($(string "[ebuild")
     >> many (satisfy (`notElem` "^\\[]D"))
     >> $(char 'D')
     >> many (satisfy (`notElem` "^\\[]"))
     >> $(char ']')
     >> pure True)
    <|> (many (satisfy (/= '\n')) >> $(char '\n') >> parseDowngradeByLine)
    <|> (eof >> pure False)

categoryAlter :: Text -> Maybe (HashSet Text) -> Maybe (HashSet Text)
categoryAlter name Nothing = Just . Set.singleton $ name
categoryAlter name s       = fmap (Set.insert name) s

toDate :: SystemTime -> Text
toDate = T.pack . formatTime defaultTimeLocale "%c" . systemToUTCTime

prettyPackage :: Package -> Text
prettyPackage (category, name) = T.append category . T.cons '/' $ name

filePathPackage :: Package -> String
filePathPackage (category, name) =
  T.unpack category ++ '-' : T.unpack name ++ ".log"

randomPackage :: StdGen -> PackageMap -> (Package, StdGen)
randomPackage g packageMap = ((category, name), g'')
  where
    (index, g') = randomR (0, Map.size packageMap - 1) g
    category = keys packageMap !! index
    packageSet = packageMap ! category
    (index', g'') = randomR (0, Set.size packageSet - 1) g'
    name = Set.toList packageSet !! index'

getUntried :: PackageMap -> St -> St
getUntried packages st = st {untried = packages'}
  where
    packages' = Map.differenceWith diffMaybe packages . tried $ st
    diffMaybe from remove
      | Set.null . Set.difference from $ remove = Nothing
      | otherwise = Just . Set.difference from $ remove

sizeMap :: PackageMap -> Int
sizeMap = sum . map Set.size . elems

failedResolve :: Package -> St -> St
failedResolve (category, name) st =
  st {failed = alter (insertIf name) category . failed $ st}

hasDowngraded :: Package -> St -> St
hasDowngraded (category, name) st =
  st {downgrade = alter (insertIf name) category . downgrade $ st}

hasCompleted :: Package -> St -> St
hasCompleted (category, name) st =
  st {completed = alter (insertIf name) category . completed $ st}

hasFailed :: Package -> St -> St
hasFailed (category, name) st =
  st {failed = alter (insertIf name) category . failed $ st}

addTried :: Package -> St -> St
addTried (category, name) st =
  st {tried = alter (insertIf name) category . tried $ st}

insertIf :: Text -> Maybe (HashSet Text) -> Maybe (HashSet Text)
insertIf name Nothing    = Just (Set.singleton name)
insertIf name (Just set) = Just (Set.insert name set)
