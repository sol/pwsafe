module Action (edit, dump) where

import           System.IO

import           Options (Options)
import qualified Options
import qualified Database
import           Util (run, withTempFile)

edit :: Options -> IO ()
edit Options.Options {Options.databaseFile = databaseFile} = withTempFile $ \fn h -> do
  putStrLn $ "using temporary file: " ++ fn
  Database.decrypt databaseFile >>= hPutStr h >> hClose h
  run "vim" ["-n", "-i", "NONE", "-c", "set nobackup", "-c", "set ft=dosini", fn]
  readFile fn >>= Database.encrypt databaseFile
  run "shred" [fn]

dump :: Options -> IO ()
dump opts = Database.decrypt databaseFile >>= putStr
  where databaseFile = Options.databaseFile opts
