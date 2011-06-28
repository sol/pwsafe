module Main (main) where

import           System.Environment (getArgs)
import           Control.Exception (finally)
import           System.Exit

import           Util (ifM)
import           Options (Mode(..))
import qualified Options
import qualified Lock
import qualified Action

main :: IO ()
main = do
  opts <- getArgs >>= Options.get
  case Options.mode opts of
    Help        ->            Options.printHelp
    Add url     -> withLock $ Action.add url opts
    Query s     ->            Action.query s opts
    List        ->            Action.list opts
    Edit        -> withLock $ Action.edit opts
    Dump        ->            Action.dump opts
    AcquireLock -> ifM Lock.acquire exitSuccess failOnLock
    ReleaseLock -> ifM Lock.release exitSuccess exitFailure
  where
    withLock action = ifM Lock.acquire (action `finally` Lock.release) failOnLock
    failOnLock = fail "Acquiring lock failed!"
