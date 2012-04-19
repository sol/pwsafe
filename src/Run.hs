module Run (run) where
import           Control.Exception (finally)
import           System.Exit
import           System.IO (Handle)

import           Util (ifM)
import           Options (Mode(..))
import qualified Options
import qualified Lock
import           Config (Config)
import qualified Action
import           Cipher (Cipher)

run :: Config -> (FilePath -> Cipher) -> Handle -> [String] -> IO ()
run conf cipher h args = do
  opts <- Options.get args
  let c = cipher $ Options.databaseFile opts
      runAction = Action.runAction (Action.mkEnv conf c h)
  case Options.mode opts of
    Help        ->            Options.printHelp
    Add url     -> withLock $ runAction $ Action.add   url (Options.userName opts)
    Query s     ->            runAction $ Action.query s (maybe 1 id $ Options.repeatCount opts)
    List p      ->            runAction $ Action.list  p
    Edit        -> withLock $             Action.edit  c
    Dump        ->            runAction $ Action.dump
    AcquireLock -> ifM Lock.acquire exitSuccess failOnLock
    ReleaseLock -> ifM Lock.release exitSuccess exitFailure
  where
    withLock action = ifM Lock.acquire (action `finally` Lock.release) failOnLock
    failOnLock = error "Acquiring lock failed!"
