module Database (readDB, writeDB, Entry(..)) where

import Control.Monad (when)
import Control.Exception (evaluate)

import System.Environment (getEnv)
import System.FilePath (joinPath)
import System.IO (hGetContents, hPutStrLn, hFlush, hClose)
import System.Process
import System.Exit

data Entry = Entry {
  entryLogin    :: String
, entryPassword :: String
, entryUrl      :: String
} deriving (Show, Read)

getDatabasePath :: IO String
getDatabasePath = do
  home <- getEnv "HOME"
  return $ joinPath [home, ".pwsafe", "db"]

readDB :: IO [Entry]
readDB = do
  filename <- getDatabasePath
  (Nothing, Just outh, Nothing, pid) <- createProcess $ (proc "gpg" ["-d", filename]) {std_out = CreatePipe}
  output <- hGetContents outh
  _ <- evaluate $ length output
  hClose outh
  e <- waitForProcess pid
  when (e /= ExitSuccess) $ fail $ "gpg exited with an error: " ++ show e 
  return $ read output
  
writeDB :: [Entry] -> IO ()
writeDB db = do
  filename <- getDatabasePath
  (Just inh, Nothing, Nothing, pid) <- createProcess $ (proc "gpg" ["-e", "-a", "--default-recipient-self", "--output", filename]) {std_in = CreatePipe}
  hPutStrLn inh $ show db
  hFlush inh
  hClose inh
  e <- waitForProcess pid
  when (e /= ExitSuccess) $ fail $ "gpg exited with an error: " ++ show e 
  return ()
