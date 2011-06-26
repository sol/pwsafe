module Database (Database, readDB, addEntry, Entry(..), lookupEntry) where

import Control.Monad (when)
import Control.Exception (evaluate)

import Text.Printf (printf)

import System.IO (hGetContents, hPutStr, hFlush, hClose)
import System.Process
import System.Exit

import qualified Data.Map as Map
import           Data.Map (Map)

import System.Directory (renameFile)

import qualified Data.Ini.Reader as Ini

data Entry = Entry {
  entryName     :: String
, entryLogin    :: String
, entryPassword :: String
, entryUrl      :: String
} deriving Show

data Database = Database {
    entries       :: Map String Entry
  , source        :: String
  , fileName      :: FilePath
} deriving Show

lookupEntry :: Database -> String -> Maybe Entry
lookupEntry db s = Map.lookup s $ entries db

readDB :: FilePath -> IO Database
readDB filename = do
  (Nothing, Just outh, Nothing, pid) <- createProcess $ (proc "gpg" ["-d", filename]) {std_out = CreatePipe}
  output <- hGetContents outh
  _ <- evaluate $ length output
  hClose outh
  e <- waitForProcess pid
  when (e /= ExitSuccess) $ fail $ "gpg exited with an error: " ++ show e 

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


addEntry :: Database -> Entry -> IO Database
addEntry db entry = do

  let source_ = source db ++ renderEntry entry
  let db_ = db {entries = Map.insert (entryName entry) (entry) $ entries db, source = source_}

  let f = fileName db
  renameFile f $ f ++ ".old"

  (Just inh, Nothing, Nothing, pid) <- createProcess $ (proc "gpg" ["-e", "-a", "--default-recipient-self", "--output", f]) {std_in = CreatePipe}
  hPutStr inh source_
  hFlush inh
  hClose inh
  e <- waitForProcess pid
  when (e /= ExitSuccess) $ fail $ "gpg exited with an error: " ++ show e 
  return db_
  where
    renderEntry (Entry {entryName = name, entryLogin = login, entryPassword = password, entryUrl = url}) =
      printf "\n[%s]\nlogin=%s\npassword=%s\nurl=%s\n" name login password url
