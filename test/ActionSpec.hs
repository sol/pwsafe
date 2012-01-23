{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-missing-signatures -fno-warn-orphans #-}
module ActionSpec (main, spec) where

import           Prelude hiding (catch)
import           Test.Spec
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC
import           Control.Monad.IO.Class
import           Data.String.Builder
import           Data.List (sort)

import           Database (entryName)
import qualified Main
import           Data.IORef
import           Cipher
import qualified Data.Knob as K
import qualified Data.ByteString.Char8 as B
import           System.IO
import           Config
import           DatabaseSpec (DatabaseFile(..))

instance (MonadIO m) => MonadIO (QC.PropertyM m) where
  liftIO = QC.run . liftIO

testConfig = Config {
  copyToClipboard  = const $ return ()
, openUrl          = const $ return ()
, generateUser     = return "default user"
, generatePassword = return "default password"
}

pwsafe :: Config -> String -> String -> IO (String, String)
pwsafe conf args db = do
  c <- idCipher
  encrypt c db
  k <- K.newKnob ""
  K.withFileHandle k "knob.txt" WriteMode $ \h -> do
    Main.run conf (const c) h (words args)
  db_ <- Cipher.decrypt c
  out <- B.unpack `fmap` K.getContents k
  return (out, db_)

idCipher :: IO Cipher
idCipher = do
  ref <- newIORef ""
  return $ Cipher (writeIORef ref) (readIORef ref)

shouldBeBuilder a b = a `shouldBe` build b

actual `shouldBeQC` expected = do
  if actual == expected then return () else do
    fail $ "expected: " ++ show expected ++ "\n but got: " ++ show actual


main = run spec

spec = do
  describe "list" $ do
    it "works on a config with one entry" testCase $ do
      (r, _) <- pwsafe testConfig "--list" $ build $ do
        "[example.com]"
        "user=foo"
        "password=bar"
        "url=http://example.com"
      r `shouldBe` "  example.com\n"

    it "works on a config with arbitrary entries" testProperty $ \(DatabaseFile db xs) -> QC.monadicIO $ do
      (r, _) <- liftIO $ pwsafe testConfig "--list" db
      let expected = unlines $ sort $ map (("  " ++) . entryName) xs
      r `shouldBeQC` expected

  describe "add" $ do
    it "adds an entry to an empty config" testCase $ do
      (_, r) <- pwsafe testConfig "--add http://example.com/" ""
      r `shouldBeBuilder` do
        "[example.com]"
        "user=default user"
        "password=default password"
        "url=http://example.com/"

    it "adds an entry to a config with one entry" testCase $ do
      (_, r) <- pwsafe testConfig "--add http://example.com/" $ build $ do
        "[foobar.com]"
        "user=foo"
        "password=bar"
      r `shouldBeBuilder` do
        "[foobar.com]"
        "user=foo"
        "password=bar"
        ""
        "[example.com]"
        "user=default user"
        "password=default password"
        "url=http://example.com/"

    it "adds an entry to an arbitrary config" testProperty $ \(DatabaseFile db xs) -> all ((/= "example.com") . entryName) xs ==> QC.monadicIO $ do
      (_, r) <- liftIO $ pwsafe testConfig "--add http://example.com/" db
      entry <- return . build $ do
        "[example.com]"
        "user=default user"
        "password=default password"
        "url=http://example.com/"
      let expected = if null db then entry else db ++ "\n" ++ entry
      r `shouldBeQC` expected

    it "fails on duplicate entry" testCase $ do
      c <- return . build $ do
        "[example.com]"
        "user=foo"
        "password=bar"
        "url=http://example.com"
      pwsafe testConfig "--add http://example.com/" c `shouldThrow` errorCall "Entry with name \"example.com\" already exists!"

  describe "idCipher (test helper)" $ do
    it "can encrypt and decrypt" testProperty $ \s -> QC.monadicIO $ do
      r <- liftIO $ do
        c <- idCipher
        encrypt c s
        decrypt c
      r `shouldBeQC` s
