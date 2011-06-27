module Main (main) where

import System.Environment (getArgs)
import System.Process
import Control.Exception (finally)
import System.Exit

import           Control.DeepSeq (deepseq)

import           Database (Entry(..))
import qualified Database

import           Util (nameFromUrl, ifM)

import qualified Options
import           Options (Options, Mode(..))

import qualified Lock

main :: IO ()
main = do
  opts <- getArgs >>= Options.get
  case Options.mode opts of
    Help        -> Options.printHelp
    Add url     -> withLock $ add url opts
    Query s     -> query s opts
    List        -> listEntries opts
    AcquireLock -> ifM Lock.acquire exitSuccess failOnLock
    ReleaseLock -> ifM Lock.release exitSuccess exitFailure
  where
    withLock action = ifM Lock.acquire (action `finally` Lock.release) failOnLock
    failOnLock = fail "Acquiring lock failed!"


listEntries :: Options -> IO ()
listEntries opts = do
  db <- Database.readDB $ Options.databaseFile opts
  mapM_ putStrLn $ Database.entrieNames db

query :: String -> Options -> IO ()
query kw opts = do
  db <- Database.readDB $ Options.databaseFile opts
  case Database.lookupEntry db kw of
    Nothing -> putStrLn "no match"
    Just x  -> do
      putStrLn $ entryUrl x
      open (entryUrl x)
      xclip (entryLogin x)
      xclip (entryPassword x)
  where
    xclip :: String -> IO ()
    xclip input = readProcess "xclip" ["-l", "2", "-quiet"] input >> return ()

    open :: String -> IO ()
    open url = rawSystem "gnome-open" [url] >> return ()

add :: String -> Options -> IO ()
add url_ opts = do
  login_ <- genLogin
  password_ <- genPassword
  addEntry $ Entry {entryName = nameFromUrl url_, entryLogin = login_, entryPassword = password_, entryUrl = url_}
  where
    genLogin :: IO String
    genLogin = fmap init $ readProcess "pwgen" ["-s"] ""

    genPassword :: IO String
    genPassword = fmap init $ readProcess "pwgen" ["-s", "20"] ""

    addEntry :: Entry -> IO ()
    addEntry entry =
      -- An Entry may have pending exceptions (e.g. invalid URL), so we force
      -- them with `deepseq` before we read the database.  This way the user
      -- gets an error before he has to enter his password.
      entry `deepseq` do
        db <- Database.readDB $ Options.databaseFile opts
        case Database.addEntry db entry of
          Left err  -> fail err
          Right db_ -> Database.save db_
