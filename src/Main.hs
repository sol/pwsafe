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
  run args Cipher.gpgCipher

run :: [String] -> Cipher -> IO ()
run args c = do
  opts <- Options.get args
  case Options.mode opts of
    Help        ->            Options.printHelp
    Add url     -> withLock $ Action.add   c url opts
    Query s     ->            Action.query c s opts
    List        ->            Action.list  c opts
    Edit        -> withLock $ Action.edit  c opts
    Dump        ->            Action.dump  c opts
    AcquireLock -> ifM Lock.acquire exitSuccess failOnLock
    ReleaseLock -> ifM Lock.release exitSuccess exitFailure
  where
    withLock action = ifM Lock.acquire (action `finally` Lock.release) failOnLock
    failOnLock = fail "Acquiring lock failed!"
