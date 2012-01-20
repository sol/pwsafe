module Main (main, run) where

import           System.Environment (getArgs)
import           Control.Exception (finally)
import           System.Exit

import           Util (ifM)
import           Options (Mode(..))
import qualified Options
import qualified Lock
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
  case Options.mode opts of
    Help        ->            Options.printHelp
    Add url     -> withLock $ Action.add   c url
    Query s     ->            Action.query c s
    List        ->            Action.list  c
    Edit        -> withLock $ Action.edit  c
    Dump        ->            Action.dump  c
    AcquireLock -> ifM Lock.acquire exitSuccess failOnLock
    ReleaseLock -> ifM Lock.release exitSuccess exitFailure
  where
    withLock action = ifM Lock.acquire (action `finally` Lock.release) failOnLock
    failOnLock = fail "Acquiring lock failed!"
