{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Action (runAction, mkEnv, add, query, list, edit, dump) where

import           Prelude hiding (putStrLn, putStr)
import           Control.Monad (replicateM_, unless)
import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class (liftIO)
import           System.IO (hPutStr, hClose)
import qualified System.IO as IO
import           Control.DeepSeq (deepseq)
import           Text.Printf
import           Data.Foldable (forM_)

import           Util (nameFromUrl, run, withTempFile, match_)
import           Database (Database, Entry(..))
import qualified Database
import           Cipher (Cipher)
import qualified Cipher
import           Config (Config)
import qualified Config

newtype ActionM a = ActionM (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad)

runAction :: Env -> ActionM a -> IO a
runAction env (ActionM a) = runReaderT a env

data Env = Env {
  envConfig :: Config
, envCipher :: Cipher
, envHandle :: IO.Handle
}

mkEnv :: Config -> Cipher -> IO.Handle -> Env
mkEnv conf cipher h = Env {
  envConfig = conf
, envCipher = cipher
, envHandle = h
}

liftAction :: (Env -> IO a) -> ActionM a
liftAction action = ActionM $ do
  env <- ask
  liftIO $ action env

liftAction1 :: (Env -> b -> IO a) -> b -> ActionM a
liftAction1 action x = ActionM $ do
  env <- ask
  liftIO $ action env x

putStrLn :: String -> ActionM ()
putStrLn = liftAction1 (IO.hPutStrLn . envHandle)

putStr :: String -> ActionM ()
putStr = liftAction1 (IO.hPutStr . envHandle)

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


add :: String -> Maybe String -> ActionM ()
add url_ mUser = do
  user <- maybe genUser return mUser
  password_ <- genPassword
  addEntry $ Entry {entryName = nameFromUrl url_, entryUser = Just user, entryPassword = Just password_, entryUrl = Just url_}
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
          Left err  -> error err
          Right db_ -> saveDatabase db_

query :: String -> Int -> Bool -> ActionM ()
query kw n passwordOnly = do
  db <- openDatabase
  case Database.lookupEntry db kw of
    Left err -> putStrLn err
    Right x  -> x `deepseq` do -- force pending exceptions early..
      unless passwordOnly $ do
        forM_ (entryUrl x) $ \url -> do
          putStrLn url
          open url
        forM_ (entryUser x)
          copyToClipboard
      forM_ (entryPassword x) $
        replicateM_ n . copyToClipboard
  where
    open = liftAction1 (Config.openUrl . envConfig)

list :: Maybe String -> ActionM ()
list mPattern = do
  db <- openDatabase
  let names = Database.entryNames db
  mapM_ (putStrLn . printf "  %s") $ maybe names (flip match_ names) mPattern

edit :: Cipher -> IO ()
edit c = withTempFile $ \fn h -> do
  IO.putStrLn $ "using temporary file: " ++ fn
  Cipher.decrypt c >>= hPutStr h >> hClose h
  run "vim" ["-n", "-i", "NONE", "-c", "set nobackup", "-c", "set ft=dosini", fn]
  readFile fn >>= Cipher.encrypt c
  run "shred" [fn]

dump :: ActionM ()
dump = decrypt >>= putStr
