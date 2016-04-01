module Options where

import Control.Monad
import System.Exit
import System.Environment (getEnv)
import System.FilePath (joinPath)
import System.Console.GetOpt
import Text.Printf (printf)

data Mode = Help | Add String | Query String | List (Maybe String) | Edit | Dump
  deriving (Eq, Show)

data Options = Options {
    mode          :: Mode
  , databaseFile  :: FilePath
  , userName      :: Maybe String
  , repeatCount   :: Maybe Int
  , passwordOnly  :: Bool
  } deriving Show

defaultOptions :: Options
defaultOptions  = Options {
    mode          = Help
  , databaseFile  = ""
  , userName      = Nothing
  , repeatCount   = Nothing
  , passwordOnly  = False
  }

options :: [OptDescr (Options -> Options)]
options = [
    Option []     ["help"]    (NoArg  (\  opts -> opts { mode = Help }))            "display this help and exit"
  , Option ['a']  ["add"]     (ReqArg (\s opts -> opts { mode = Add s })   "URL")   "add a new entry to the database; the password is\nalways automatically generated; the username is\ngenerated unless --user is specified"
  , Option ['q']  ["query"]   (ReqArg (\s opts -> opts { mode = Query s }) "TERM")  "lookup a password, the term must match exactly one\nentry"
  , Option ['l']  ["list"]    (OptArg (\s opts -> opts { mode = List s})   "TERM") "list all entries matching the given term"
  , Option ['e']  ["edit"]    (NoArg  (\  opts -> opts { mode = Edit}))             "invoke vim to edit the database using sensible\ndefaults (no backup, no swapfile etc)"
  , Option []     ["dump"]    (NoArg  (\  opts -> opts { mode = Dump}))             "dump database to stdout"

  , Option []     ["dbfile"]  (ReqArg (\s opts -> opts { databaseFile = s }) "FILE")  "file where passwords are stored;\ndefaults to ~/.pwsafe/db"
  , Option []     ["user"]    (ReqArg (\s opts -> opts { userName = Just s }) "USER") "specify a username to be used for a new entry;\nthis option is to be used with --add"
  , Option ['n']  []          (ReqArg (\s opts -> opts { repeatCount = (Just . read) s }) "NUMBER") "copy password n times to clipboard;\ndefaults to 1"
  , Option []     ["password-only"]
                              (NoArg  (\  opts -> opts { passwordOnly = True}))       "only copy password to clipboard"
  ]

defaultDatabaseFile :: IO String
defaultDatabaseFile = do
  home <- getEnv "HOME"
  return $ joinPath [home, ".pwsafe", "db"]

get :: [String] -> IO Options
get args = do
  let (opts_, files, errors) = getOpt Permute options args
  let opts__ = foldl (flip id) defaultOptions opts_
  opts <- case databaseFile opts__ of
    "" -> do
      db <- defaultDatabaseFile
      return opts__ {databaseFile = db}
    _  -> return opts__

  when ((not . null) errors)
    (tryHelp $ head errors)

  when ((not . null) files)
    (tryHelp $ printf "unrecognized option `%s'\n" $ head files)

  return opts

  where
    printAndExit :: String -> IO a
    printAndExit s = putStr s >> exitFailure

    tryHelp message = printAndExit $ "pwsafe: " ++ message
      ++ "Try `pwsafe --help' for more information.\n"

printHelp :: IO ()
printHelp = putStr $ usageInfo "Usage: pwsafe [OPTION]...\n" options
