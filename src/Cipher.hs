module Cipher (Cipher(..), gpgCipher) where

import           System.IO
import           System.Process
import           System.Exit
import           System.Directory
import           Control.Monad
import           Control.DeepSeq

data Cipher = Cipher {
  encrypt :: String -> IO ()
, decrypt :: IO String
}

gpgCipher :: FilePath -> Cipher
gpgCipher filename = Cipher enc dec
  where
    enc s = do
      renameFile filename $ filename ++ ".old" -- backup file
      (Just inh, Nothing, Nothing, pid) <-
        createProcess $ (proc "gpg" ["--batch", "-e", "-a", "--default-recipient-self", "--output", filename]) {std_in = CreatePipe}
      hPutStr inh s
      hClose inh
      e <- waitForProcess pid
      when (e /= ExitSuccess) $ error $ "gpg exited with an error: " ++ show e

    dec = do
      (Nothing, Just outh, Nothing, pid) <- createProcess $ (proc "gpg" ["-d", filename]) {std_out = CreatePipe}
      output <- hGetContents outh
      output `deepseq` hClose outh
      e <- waitForProcess pid
      when (e /= ExitSuccess) $ error $ "gpg exited with an error: " ++ show e
      return output
