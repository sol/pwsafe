module Options where

import Control.Monad
import System.Exit
import System.Console.GetOpt
import Text.Printf (printf)

data Mode = Query String | Add String | Help
  deriving (Eq, Show)

data Options = Options {
    mode          :: Mode
  } deriving Show

defaultOptions :: Options
defaultOptions  = Options {
    mode          = Help
  }

options :: [OptDescr (Options -> Options)]
options = [
    Option []     ["help"]    (NoArg  (\  opts -> opts {mode = Help}))              "display this help and exit"
  , Option ['a']  ["add"]     (ReqArg (\s opts -> opts { mode = Add s })   "URL")   ""
  , Option ['q']  ["query"]   (ReqArg (\s opts -> opts { mode = Query s }) "TERM")  ""
  ]

get :: [String] -> IO Options
get args = do
  let (opts_, files, errors) = getOpt Permute options args
  let opts = foldl (flip id) defaultOptions opts_

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
