module Action (add, query, list, edit, dump) where

import           System.IO
import           System.Process
import           Control.DeepSeq (deepseq)
import           Text.Printf

import           Data.Foldable (forM_)

import           Util (nameFromUrl, run, withTempFile)
import           Options (Options)
import qualified Options
import           Database (Entry(..))
import qualified Database
import           Cipher (Cipher)
import qualified Cipher

xclip :: String -> IO ()
xclip input = readProcess "xclip" ["-l", "1", "-quiet"] input >> return ()
-- vimperator, for some reason, needs -l 2, pentadactyl works with -l 1
-- xclip input = readProcess "xclip" ["-l", "2", "-quiet"] input >> return ()

add :: Cipher -> String -> Options -> IO ()
add c url_ opts = do
  login_ <- genLogin
  password_ <- genPassword
  addEntry $ Entry {entryName = nameFromUrl url_, entryLogin = Just login_, entryPassword = password_, entryUrl = Just url_}
  xclip login_
  xclip password_
  xclip password_
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
        db <- Database.open c $ Options.databaseFile opts
        case Database.addEntry db entry of
          Left err  -> fail err
          Right db_ -> Database.save c db_

query :: Cipher -> String -> Options -> IO ()
query c kw opts = do
  db <- Database.open c $ Options.databaseFile opts
  case Database.lookupEntry db kw of
    Left err -> putStrLn err
    Right x  -> x `deepseq` do -- force pending exceptions early..

      forM_ (entryUrl x) $ \url -> do
        putStrLn url
        open url

      case entryLogin x of
        Nothing -> putStrLn "no login, skipping"
        Just l  -> xclip l
      xclip (entryPassword x)
  where

    open :: String -> IO ()
    open url = run "xdg-open" [url]

list :: Cipher -> Options -> IO ()
list c opts = do
  db <- Database.open c $ Options.databaseFile opts
  mapM_ (putStrLn . printf "  %s") $ Database.entryNames db

edit :: Cipher -> Options -> IO ()
edit c Options.Options {Options.databaseFile = databaseFile} = withTempFile $ \fn h -> do
  putStrLn $ "using temporary file: " ++ fn
  Cipher.decrypt c databaseFile >>= hPutStr h >> hClose h
  run "vim" ["-n", "-i", "NONE", "-c", "set nobackup", "-c", "set ft=dosini", fn]
  readFile fn >>= Cipher.encrypt c databaseFile
  run "shred" [fn]

dump :: Cipher -> Options -> IO ()
dump c opts = Cipher.decrypt c databaseFile >>= putStr
  where databaseFile = Options.databaseFile opts
