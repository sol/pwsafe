module CipherSpec (main, spec) where

import           Test.Hspec.ShouldBe

import           Prelude hiding (catch)
import           Control.Exception (catch)
import           System.Directory

import qualified Cipher

main :: IO ()
main = hspec spec

testDbFilename :: String
testDbFilename = "./test.db"

testCipher :: Cipher.Cipher
testCipher = (Cipher.gpgCipher ["--homedir", "test/dot-gnupg"]) testDbFilename

spec :: Spec
spec = do

  describe "gpgCipher decrypt" $ do
    it "returns the empty string when the database does not exist" $ do
      removeFile testDbFilename `catch` handlerUnit
      (Cipher.decrypt testCipher) `shouldReturn` ""

  describe "gpgCipher" $ do
    it "encrypt followed by decrypt works when the database does not exist" $ do
      removeFile testDbFilename `catch` handlerUnit
      Cipher.encrypt testCipher "foobar"
      Cipher.decrypt testCipher `shouldReturn` "foobar"
      removeFile testDbFilename
      removeFile "test/dot-gnupg/random_seed"

handlerUnit :: IOError -> IO ()
handlerUnit = const $ return ()
