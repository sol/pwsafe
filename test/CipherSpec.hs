module CipherSpec (main, spec) where

import           Test.Hspec.ShouldBe
import qualified Cipher
import           System.Directory
import           Control.Exception (catch)
import           Prelude hiding (catch)

main :: IO ()
main = hspecX spec

testDbFilename :: String
testDbFilename = "./test.db"

testCipher :: Cipher.Cipher
testCipher = (Cipher.gpgCipher ["--homedir", "test/data"]) testDbFilename

spec :: Specs
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
      removeFile "test/data/random_seed"

handlerUnit :: IOError -> IO ()
handlerUnit = const $ return ()
