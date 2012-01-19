module Cipher (Cipher, encrypt, decrypt, idCipher, gpgCipher) where

import           System.IO
import           System.Process
import           System.Exit
import           System.Directory
import           Control.Monad
import           Control.DeepSeq
import           Text.Printf
import           Util (ifM)

data Cipher = Cipher {
  encrypt :: FilePath -> String -> IO ()
, decrypt :: FilePath -> IO String
}

idCipher :: Cipher
idCipher = Cipher writeFile readFile

gpgCipher :: Cipher
gpgCipher = Cipher enc dec
  where
    enc :: FilePath -> String -> IO ()
    enc f s = do
      renameFile f $ f ++ ".old" -- backup file
      (Just inh, Nothing, Nothing, pid) <-
        createProcess $ (proc "gpg" ["--batch", "-e", "-a", "--default-recipient-self", "--output", f]) {std_in = CreatePipe}
      hPutStr inh s
      hClose inh
      e <- waitForProcess pid
      when (e /= ExitSuccess) $ fail $ "gpg exited with an error: " ++ show e

    dec :: FilePath -> IO String
    dec filename = do
      (Nothing, Just outh, Nothing, pid) <- createProcess $ (proc "gpg" ["-d", filename]) {std_out = CreatePipe}
      output <- hGetContents outh
      output `deepseq` hClose outh
      e <- waitForProcess pid
      when (e /= ExitSuccess) $ fail $ "gpg exited with an error: " ++ show e
      return output
