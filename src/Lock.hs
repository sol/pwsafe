{-# LANGUAGE ViewPatterns #-}
module Lock (withLock) where
import           Control.Exception
import           System.IO
import           System.Exit
import           System.FileLock

withLock :: FilePath -> IO a -> IO a
withLock ((++ ".lock") -> file) action = do
  bracket (tryLockFile file Exclusive) (maybe (return ()) unlockFile) $ \lock -> do
    case lock of
      Just _ -> do
        action
      Nothing -> do
        hPutStrLn stderr $ "Acquiring " ++ file ++ " failed!"
        exitFailure
