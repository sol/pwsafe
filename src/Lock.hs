-- | A mutually exclusive lock
module Lock (acquire, release) where

import System.Posix.Files (ownerReadMode, ownerWriteMode, unionFileModes)
import System.Posix.Semaphore
import Control.Exception (catch, SomeException)
import Prelude hiding (catch)

-- | Try to acquire the lock, return `True` on success
acquire :: IO Bool
acquire = (semOpen "pwsafe" (OpenSemFlags True True) readWriteMode 0 >> return True) `catch` handlerFalse
  where
    readWriteMode = ownerReadMode `unionFileModes` ownerWriteMode

-- | Release the lock, return `True` on success
release :: IO Bool
release = (semUnlink "pwsafe" >> return True) `catch` handlerFalse

-- | Return `True` on `SomeException`
handlerFalse :: SomeException -> IO Bool
handlerFalse = const $ return False
