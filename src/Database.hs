module Database (Database, open, save, addEntry, Entry(..), lookupEntry, entryNames) where

import           Prelude hiding (lookup)

import           Data.List (intercalate)
import           Control.DeepSeq
import           Text.Printf (printf)

import           Data.Config.String   (Config)
import qualified Data.Config.String as Config

import           Util (match, MatchResult(..))
import           Cipher (Cipher)
import qualified Cipher

data Entry = Entry {
  entryName     :: String
, entryLogin    :: Maybe String
, entryPassword :: String
, entryUrl      :: Maybe String
} deriving (Eq, Show)

instance NFData Entry where
  rnf (Entry x1 x2 x3 x4) = rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4

data Database = Database {
    config        :: Config
  , fileName      :: FilePath
}

lookupEntry :: Database -> String -> Either String Entry
lookupEntry db s = case match s $ entryNames db of
  None        -> Left "no match"
  Ambiguous l -> Left $ printf "ambiguous, could refer to:\n  %s" $ intercalate "\n  " l
  Match name  -> go
    where
      go = do
        password <- get "password"
        let url = lookup "url"
        return Entry {entryName = name, entryLogin = lookup "login", entryPassword = password, entryUrl = url}
      lookup k = Config.lookup name k (config db)
      get k = maybe
        (Left $ "config error: section [" ++ name ++ "] dose not define required option " ++ show k ++ "!")
        Right
        (lookup k)

hasEntry :: String -> Database -> Bool
hasEntry name = Config.hasSection name . config

entryNames :: Database -> [String]
entryNames = Config.sections . config

addEntry :: Database -> Entry -> Either String Database
addEntry db entry =
  case hasEntry name db of
    True  -> Left $ printf "Entry with name \"%s\" already exists!" name
    False -> Right db {config = insertEntry entry $ config db}
  where
    name      = entryName entry

insertEntry :: Entry -> Config -> Config
insertEntry entry = mInsert "url" url . insert "password" password . mInsert "login" login
  where
    insert = Config.insert $ entryName entry
    mInsert k = maybe id (insert k)

    login     = entryLogin entry
    password  = entryPassword entry
    url       = entryUrl entry

open :: Cipher -> FilePath -> IO Database
open c filename = do
  Cipher.decrypt c filename >>= \input ->
    case Config.parse input of
      Left err ->
        error err
      Right conf ->
        return Database {
            config = conf
          , fileName = filename
        }

save :: Cipher -> Database -> IO ()
save c db = Cipher.encrypt c (fileName db) (source db)
  where
    source = Config.render . config
