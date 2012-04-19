module Main (main, run) where

import           System.Environment (getArgs)
import           System.IO (stdout)

import           Config (defaultConfig)
import qualified Cipher
import           Run (run)

main :: IO ()
main = do
  args <- getArgs
  run defaultConfig (Cipher.gpgCipher []) stdout args
