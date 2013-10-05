-- | A mutually exclusive lock
module Lock (acquire, release) where

import           System.Posix.Files (ownerReadMode, ownerWriteMode, unionFileModes)
import           System.Posix.Semaphore (semOpen, OpenSemFlags (..), semUnlink)
import qualified Control.Exception as E

-- | Try to acquire the lock, return `True` on success
acquire :: IO Bool
acquire = (semOpen "/pwsafe" (OpenSemFlags True True) readWriteMode 0 >> return True) `E.catch` handlerFalse
  where
    readWriteMode = ownerReadMode `unionFileModes` ownerWriteMode

-- | Release the lock, return `True` on success
release :: IO Bool
release = (semUnlink "/pwsafe" >> return True) `E.catch` handlerFalse

-- | Return `False` on `E.SomeException`
handlerFalse :: E.SomeException -> IO Bool
handlerFalse = const $ return False
