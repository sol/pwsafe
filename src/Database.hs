module Database (readDB, writeDB, Entry(..)) where

import Control.Monad (when)
import Control.Exception (evaluate)

import Text.Printf (printf)

import System.IO (hGetContents, hPutStrLn, hFlush, hClose)
import System.Process
import System.Exit

import qualified Data.Ini.Reader as Ini
import qualified Data.Ini as Ini

data Entry = Entry {
  entryName     :: String
, entryLogin    :: String
, entryPassword :: String
, entryUrl      :: String
} deriving Show

readDB :: FilePath -> IO [Entry]
readDB filename = do
  (Nothing, Just outh, Nothing, pid) <- createProcess $ (proc "gpg" ["-d", filename]) {std_out = CreatePipe}
  output <- hGetContents outh
  _ <- evaluate $ length output
  hClose outh
  e <- waitForProcess pid
  when (e /= ExitSuccess) $ fail $ "gpg exited with an error: " ++ show e 
  return $ parseResultToEntries $ Ini.parse output
  where
    parseResultToEntries (Left err)  = error $ show err
    parseResultToEntries (Right c) = map sectionToEntry (Ini.sections c)
      where
        sectionToEntry s = Entry {entryName = s, entryLogin = get "login", entryPassword = get "password", entryUrl = get "url"}
          where
            get k = case Ini.getOption s k c of
              Nothing -> error $ "config error: section " ++ show s ++ "] dose not define required option " ++ show k ++ "!"
              Just x -> x


writeDB :: FilePath -> [Entry] -> IO ()
writeDB filename db = do
  (Just inh, Nothing, Nothing, pid) <- createProcess $ (proc "gpg" ["-e", "-a", "--default-recipient-self", "--output", filename]) {std_in = CreatePipe}
  mapM_ (hPutStrLn inh . renderEntry) db
  hFlush inh
  hClose inh
  e <- waitForProcess pid
  when (e /= ExitSuccess) $ fail $ "gpg exited with an error: " ++ show e 
  return ()
  where
    renderEntry (Entry {entryName = name, entryLogin = login, entryPassword = password, entryUrl = url}) =
      printf "[%s]\nlogin=%s\npassword=%s\nurl=%s\n" name login password url
