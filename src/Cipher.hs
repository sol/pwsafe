module Cipher (Cipher(..), gpgCipher) where

import           System.IO
import           System.Process
import           System.Exit
import           System.Directory
import           Control.Monad
import           Control.DeepSeq
import           Util
import           Text.Printf

data Cipher = Cipher {
  encrypt :: String -> IO ()
, decrypt :: IO String
}

gpgCipher :: FilePath -> Cipher
gpgCipher filename = Cipher enc dec
  where
    enc s = do
      getBackupFileName filename 0 >>= renameFile filename
      (Just inh, Nothing, Nothing, pid) <-
        createProcess $ (proc "gpg" ["--batch", "-e", "-a", "--default-recipient-self", "--output", filename]) {std_in = CreatePipe}
      hPutStr inh s
      hClose inh
      e <- waitForProcess pid
      when (e /= ExitSuccess) $ error $ "gpg exited with an error: " ++ show e
      where
        getBackupFileName :: FilePath -> Int -> IO FilePath
        getBackupFileName baseName n =
          ifM (doesFileExist backupFile) (getBackupFileName baseName $ succ n) (return backupFile)
          where
            backupFile = printf "%s.old.%d" baseName n

    dec = do
      (Nothing, Just outh, Nothing, pid) <- createProcess $ (proc "gpg" ["-d", filename]) {std_out = CreatePipe}
      output <- hGetContents outh
      output `deepseq` hClose outh
      e <- waitForProcess pid
      when (e /= ExitSuccess) $ error $ "gpg exited with an error: " ++ show e
      return output
