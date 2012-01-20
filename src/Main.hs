module Main (main, run) where

import           System.Environment (getArgs)
import           Control.Exception (finally)
import           System.Exit

import           Util (ifM)
import           Options (Mode(..))
import qualified Options
import qualified Lock
import           Config (defaultConfig)
import qualified Action
import           Cipher (Cipher)
import qualified Cipher

main :: IO ()
main = do
  args <- getArgs
  run Cipher.gpgCipher args

run :: (FilePath -> Cipher) -> [String] -> IO ()
run cipher args = do
  opts <- Options.get args
  let c = cipher $ Options.databaseFile opts
      runAction = Action.runAction (Action.mkEnv defaultConfig c)
  case Options.mode opts of
    Help        ->            Options.printHelp
    Add url     -> withLock $ runAction $ Action.add   url
    Query s     ->            runAction $ Action.query s
    List        ->            runAction $ Action.list
    Edit        -> withLock $             Action.edit  c
    Dump        ->            runAction $ Action.dump
    AcquireLock -> ifM Lock.acquire exitSuccess failOnLock
    ReleaseLock -> ifM Lock.release exitSuccess exitFailure
  where
    withLock action = ifM Lock.acquire (action `finally` Lock.release) failOnLock
    failOnLock = fail "Acquiring lock failed!"
