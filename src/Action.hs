module Action (add, query, list, edit, dump) where

import           System.IO
import           System.Process
import           Control.DeepSeq (deepseq)
import           Text.Printf

import           Data.Foldable (forM_)

import           Util (nameFromUrl, run, withTempFile)
import           Database (Entry(..))
import qualified Database
import           Cipher (Cipher)
import qualified Cipher

xclip :: String -> IO ()
xclip input = readProcess "xclip" ["-l", "1", "-quiet"] input >> return ()
-- vimperator, for some reason, needs -l 2, pentadactyl works with -l 1
-- xclip input = readProcess "xclip" ["-l", "2", "-quiet"] input >> return ()

add :: Cipher -> String -> IO ()
add c url_ = do
  user <- genUser
  password_ <- genPassword
  addEntry $ Entry {entryName = nameFromUrl url_, entryUser = Just user, entryPassword = password_, entryUrl = Just url_}
  xclip user
  xclip password_
  xclip password_
  where
    genUser :: IO String
    genUser = fmap init $ readProcess "pwgen" ["-s"] ""

    genPassword :: IO String
    genPassword = fmap init $ readProcess "pwgen" ["-s", "20"] ""

    addEntry :: Entry -> IO ()
    addEntry entry =
      -- An Entry may have pending exceptions (e.g. invalid URL), so we force
      -- them with `deepseq` before we read the database.  This way the user
      -- gets an error before he has to enter his password.
      entry `deepseq` do
        db <- Database.open c
        case Database.addEntry db entry of
          Left err  -> fail err
          Right db_ -> Database.save c db_

query :: Cipher -> String -> IO ()
query c kw = do
  db <- Database.open c
  case Database.lookupEntry db kw of
    Left err -> putStrLn err
    Right x  -> x `deepseq` do -- force pending exceptions early..

      forM_ (entryUrl x) $ \url -> do
        putStrLn url
        open url
      forM_ (entryUser x) xclip
      xclip (entryPassword x)
  where

    open :: String -> IO ()
    open url = run "xdg-open" [url]

list :: Cipher -> IO ()
list c = do
  db <- Database.open c
  mapM_ (putStrLn . printf "  %s") $ Database.entryNames db

edit :: Cipher -> IO ()
edit c = withTempFile $ \fn h -> do
  putStrLn $ "using temporary file: " ++ fn
  Cipher.decrypt c >>= hPutStr h >> hClose h
  run "vim" ["-n", "-i", "NONE", "-c", "set nobackup", "-c", "set ft=dosini", fn]
  readFile fn >>= Cipher.encrypt c
  run "shred" [fn]

dump :: Cipher -> IO ()
dump c = Cipher.decrypt c >>= putStr
