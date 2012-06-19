module Database (Database, empty, parse, render, addEntry, Entry(..), lookupEntry, hasEntry, entryNames) where

import           Prelude hiding (lookup)

import           Data.List (intercalate)
import           Control.DeepSeq
import           Text.Printf (printf)

import           Data.Config.String   (Config)
import qualified Data.Config.String as Config

import           Util (match, MatchResult(..))

data Entry = Entry {
  entryName     :: String
, entryUser     :: Maybe String
, entryPassword :: Maybe String
, entryUrl      :: Maybe String
} deriving (Eq, Show)

instance NFData Entry where
  rnf (Entry x1 x2 x3 x4) = rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4

newtype Database = Database { config :: Config }

empty :: Database
empty = Database Config.empty

lookupEntry :: Database -> String -> Either String Entry
lookupEntry db s = case match s $ entryNames db of
  None        -> Left "no match"
  Ambiguous l -> Left $ printf "ambiguous, could refer to:\n  %s" $ intercalate "\n  " l
  Match name  -> entry
    where
      entry = do
        return Entry {
            entryName = name
          , entryUser = lookup "user"
          , entryPassword = lookup "password"
          , entryUrl = lookup "url"
          }
      lookup k = Config.lookup name k (config db)

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
insertEntry entry =
    mInsert "user" user
  . mInsert "password" password
  . mInsert "url" url
  where
    insert = Config.insert $ entryName entry
    mInsert k = maybe id (insert k)

    user     = entryUser entry
    password = entryPassword entry
    url      = entryUrl entry

parse :: String -> Database
parse input = either error Database (Config.parse input)

render :: Database -> String
render (Database db) = Config.render db
