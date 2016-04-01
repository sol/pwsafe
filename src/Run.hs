module Run (run) where
import           System.IO (Handle)

import           Options (Mode(..))
import qualified Options
import           Lock
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
    Add url     -> withLock (Options.databaseFile opts) $ runAction $ Action.add   url (Options.userName opts)
    Query s     ->            runAction $ Action.query s (maybe 1 id $ Options.repeatCount opts) (Options.passwordOnly opts)
    List p      ->            runAction $ Action.list  p
    Edit        -> withLock (Options.databaseFile opts) $             Action.edit  c
    Dump        ->            runAction $ Action.dump
