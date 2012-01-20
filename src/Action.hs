{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Action (runAction, mkEnv, add, query, list, edit, dump) where

import           Prelude hiding (putStrLn, putStr)
import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class (liftIO)
import           System.IO (hPutStr, hClose)
import qualified System.IO as IO
import           Control.DeepSeq (deepseq)
import           Text.Printf
import           Data.Foldable (forM_)

import           Util (nameFromUrl, run, withTempFile)
import           Database (Database, Entry(..))
import qualified Database
import           Cipher (Cipher)
import qualified Cipher
import           Config (Config)
import qualified Config

newtype ActionM a = ActionM (ReaderT Env IO a)
  deriving (Monad, Functor)

runAction :: Env -> ActionM a -> IO a
runAction env (ActionM a) = runReaderT a env

data Env = Env {
  envConfig :: Config
, envCipher :: Cipher
, putStr_   :: String -> IO ()
, putStrLn_ :: String -> IO ()
}

mkEnv :: Config -> Cipher -> Env
mkEnv conf cipher = Env {
  envConfig = conf
, envCipher = cipher
, putStr_   = IO.putStr
, putStrLn_ = IO.putStrLn
}

liftAction :: (Env -> IO a) -> ActionM a
liftAction action = ActionM $ do
  env <- ask
  liftIO $ action env

liftAction1 :: (Env -> b -> IO a) -> b -> ActionM a
liftAction1 action x = ActionM $ do
  env <- ask
  liftIO $ action env x

copyToClipboard :: String -> ActionM ()
copyToClipboard = liftAction1 (Config.copyToClipboard . envConfig)

encrypt :: String -> ActionM ()
encrypt = liftAction1 $ \Env{envCipher = c} -> Cipher.encrypt c

decrypt :: ActionM String
decrypt = liftAction  $ \Env{envCipher = c} -> Cipher.decrypt c

openDatabase :: ActionM Database
openDatabase = Database.parse `fmap` decrypt

saveDatabase :: Database -> ActionM ()
saveDatabase = encrypt . Database.render

putStrLn :: String -> ActionM ()
putStrLn = liftAction1 putStrLn_

putStr :: String -> ActionM ()
putStr = liftAction1 putStr_


add :: String -> ActionM ()
add url_ = do
  user <- genUser
  password_ <- genPassword
  addEntry $ Entry {entryName = nameFromUrl url_, entryUser = Just user, entryPassword = password_, entryUrl = Just url_}
  copyToClipboard user
  copyToClipboard password_
  copyToClipboard password_
  where
    genPassword = liftAction (Config.generatePassword . envConfig)
    genUser  = liftAction (Config.generateUser . envConfig)

    addEntry entry =
      -- An Entry may have pending exceptions (e.g. invalid URL), so we force
      -- them with `deepseq` before we read the database.  This way the user
      -- gets an error before he has to enter his password.
      entry `deepseq` do
        db <- openDatabase
        case Database.addEntry db entry of
          Left err  -> fail err
          Right db_ -> saveDatabase db_

query :: String -> ActionM ()
query kw = do
  db <- openDatabase
  case Database.lookupEntry db kw of
    Left err -> putStrLn err
    Right x  -> x `deepseq` do -- force pending exceptions early..

      forM_ (entryUrl x) $ \url -> do
        putStrLn url
        open url
      forM_ (entryUser x) copyToClipboard
      copyToClipboard (entryPassword x)
  where
    open = liftAction1 (Config.openUrl . envConfig)

list :: ActionM ()
list = do
  db <- openDatabase
  mapM_ (putStrLn . printf "  %s") $ Database.entryNames db

edit :: Cipher -> IO ()
edit c = withTempFile $ \fn h -> do
  IO.putStrLn $ "using temporary file: " ++ fn
  Cipher.decrypt c >>= hPutStr h >> hClose h
  run "vim" ["-n", "-i", "NONE", "-c", "set nobackup", "-c", "set ft=dosini", fn]
  readFile fn >>= Cipher.encrypt c
  run "shred" [fn]

dump :: ActionM ()
dump = decrypt >>= putStr
