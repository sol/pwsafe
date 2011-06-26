module Main (main) where

import Data.List

import System.Environment (getArgs)
import System.Process

import Database
import Util (nameFromUrl)

import qualified Options
import           Options (Mode(..))

main :: IO ()
main = do
  opts <- getArgs >>= Options.get
  case Options.mode opts of
    Query s -> query s
    Add url -> add url
    Help    -> Options.printHelp

query :: String -> IO ()
query kw = do
  db <- readDB
  case filterDB db of
    []  -> putStrLn "no match"
    [x] -> do
      putStrLn $ entryUrl x
      open (entryUrl x)
      xclip (entryLogin x)
      xclip (entryPassword x)
    _   -> putStrLn "ambiguous"
  where
    filterDB db = filter (\e -> kw `isInfixOf` entryName e) db

    xclip :: String -> IO ()
    xclip input = readProcess "xclip" ["-l", "2", "-quiet"] input >> return ()

    open :: String -> IO ()
    open url = rawSystem "gnome-open" [url] >> return ()

add :: String -> IO ()
add url_ = do
  login_ <- genLogin
  password_ <- genPassword
  addEntry $ Entry {entryName = nameFromUrl url_, entryLogin = login_, entryPassword = password_, entryUrl = url_}
  where
    genLogin :: IO String
    genLogin = fmap init $ readProcess "pwgen" ["-s"] ""

    genPassword :: IO String
    genPassword = fmap init $ readProcess "pwgen" ["-s", "20"] ""

    addEntry :: Entry -> IO ()
    addEntry entry = do
      db <- readDB
      writeDB $ entry : db
