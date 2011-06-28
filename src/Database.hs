module Database (Database, decrypt, encrypt, open, save, addEntry, Entry(..), lookupEntry, entrieNames) where

import           Control.Monad (when)
import           Control.DeepSeq

import           System.IO
import           System.Directory (renameFile)
import           System.Process
import           System.Exit

import qualified Data.Map as Map
import           Data.Map (Map)

import           Text.Printf (printf)

import qualified Data.Ini.Reader as Ini

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

lookupEntry :: Database -> String -> Maybe Entry
lookupEntry db s = Map.lookup s $ entries db

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


decrypt :: FilePath -> IO String
decrypt filename = do
  (Nothing, Just outh, Nothing, pid) <- createProcess $ (proc "gpg" ["-d", filename]) {std_out = CreatePipe}
  output <- hGetContents outh
  output `deepseq` hClose outh
  e <- waitForProcess pid
  when (e /= ExitSuccess) $ fail $ "gpg exited with an error: " ++ show e 
  return output

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


encrypt :: FilePath -> String -> IO ()
encrypt f s = do

  -- backup db file
  renameFile f $ f ++ ".old"

  (Just inh, Nothing, Nothing, pid) <- createProcess $ (proc "gpg" ["--batch", "-e", "-a", "--default-recipient-self", "--output", f]) {std_in = CreatePipe}
  hPutStr inh s
  hClose inh
  e <- waitForProcess pid
  when (e /= ExitSuccess) $ fail $ "gpg exited with an error: " ++ show e 

save :: Database -> IO ()
save db = encrypt (fileName db) (source db)
