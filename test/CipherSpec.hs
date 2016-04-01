module CipherSpec (spec) where

import           Test.Hspec

import           System.IO.Temp
import           System.FilePath
import           System.Process

import qualified Cipher

testCipher :: FilePath -> Cipher.Cipher
testCipher dir = (Cipher.gpgCipher ["--homedir", dir </> "dot-gnupg"]) (dir </> "test.db")

withTestCipher :: (Cipher.Cipher -> IO a) -> IO a
withTestCipher action = withSystemTempDirectory "hspec" $ \dir -> do
  callProcess "cp" ["-r", "test/dot-gnupg/", dir]
  action (testCipher dir)

spec :: Spec
spec = do
  around withTestCipher $ do
    describe "decrypt" $ do
      it "is inverse to encrypt" $ \cipher -> do
        Cipher.encrypt cipher "foobar"
        Cipher.decrypt cipher `shouldReturn` "foobar"

      context "when the database does not exist" $ do
        it "returns the empty string" $ \cipher -> do
          Cipher.decrypt cipher `shouldReturn` ""
