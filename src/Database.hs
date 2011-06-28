module Database (Database, open, save, addEntry, Entry(..), lookupEntry, entrieNames) where

import           Data.List
import           Data.Map (Map, (!))
import qualified Data.Map as Map
import           Control.DeepSeq
import           Text.Printf (printf)

import qualified Data.Ini.Reader as Ini

import           Util (encrypt, decrypt, match, MatchResult(..))

data Entry = Entry {
  entryName     :: String
, entryLogin    :: String
, entryPassword :: String
, entryUrl      :: String
} deriving Show

instance NFData Entry where
  rnf (Entry x1 x2 x3 x4) = rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4

data Database = Database {
    entries       :: Map String Entry
  , source        :: String
  , fileName      :: FilePath
} deriving Show

lookupEntry :: Database -> String -> Either String Entry
lookupEntry db s = case match s $ Map.keys $ entries db of
  None        -> Left "no match"
  Ambiguous l -> Left $ printf "ambiguous, could refer to:\n  %s" $ intercalate "\n  " l
  Match x     -> Right $ entries db ! x

entrieNames :: Database -> [String]
entrieNames = Map.keys . entries

addEntry :: Database -> Entry -> Either String Database
addEntry db entry =
  case Map.member name entries_ of
    True  -> Left $ printf "Entry with name \"%s\" already exists!" name
    False -> Right db {entries = Map.insert name entry entries_, source = source_}
  where
    name      = entryName entry
    login     = entryLogin entry
    password  = entryPassword entry
    url       = entryUrl entry

    entries_  = entries db
    source_   = source db ++
      printf "\n[%s]\nlogin=%s\npassword=%s\nurl=%s\n" name login password url

open :: FilePath -> IO Database
open filename = do
  output <- decrypt filename
  let db = Database {
      entries = parseResultToEntries $ Ini.parse output
    , source = output
    , fileName = filename
    }
  return db
  where
    parseResultToEntries (Left err)  = error $ show err
    parseResultToEntries (Right c) = Map.mapWithKey sectionToEntry c
      where
        sectionToEntry s m = Entry {entryName = s, entryLogin = get "login", entryPassword = get "password", entryUrl = get "url"}
          where
            get k = Map.findWithDefault err k m
              where
                err = error $ "config error: section [" ++ s ++ "] dose not define required option " ++ show k ++ "!"

save :: Database -> IO ()
save db = encrypt (fileName db) (source db)
