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

mockSink :: IO ((a -> IO ()), IO [a])
mockSink = do
  ref <- newIORef []
  return (modifyIORef ref . (:), reverse `fmap` readIORef ref)

data PWSafeResult = PWSafeResult {
  resultOutput    :: String
, resultDatabase  :: String
, resultClipboard :: [String]
}

pwsafe :: String -> String -> IO PWSafeResult
pwsafe args db = do
  (clipboardSink, clipboardAccessor) <- mockSink
  c <- idCipher
  encrypt c db
  k <- K.newKnob ""
  K.withFileHandle k "knob.txt" WriteMode $ \h -> do
    Main.run (conf clipboardSink) (const c) h (words args)
  db_ <- Cipher.decrypt c
  out <- B.unpack `fmap` K.getContents k
  clipboard <- clipboardAccessor
  return (PWSafeResult out db_ clipboard)
  where
    conf clipboardSink = Config {
      copyToClipboard  = clipboardSink
    , openUrl          = const $ return ()
    , generateUser     = return "default user"
    , generatePassword = return "default password"
    }


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
      r <- pwsafe "--list" $ build $ do
        "[example.com]"
        "user=foo"
        "password=bar"
        "url=http://example.com"
      resultOutput r `shouldBe` "  example.com\n"

    it "works on a config with arbitrary entries" testProperty $ \(DatabaseFile db xs) -> QC.monadicIO $ do
      r <- liftIO $ pwsafe "--list" db
      let expected = unlines $ sort $ map (("  " ++) . entryName) xs
      resultOutput r `shouldBeQC` expected

    it "optionally takes a pattern" $ do
      r <- pwsafe "--list=bar" $ build $ do
        "[foo]"
        "[bar]"
        "[baz]"
        "[foobar]"
        "[foobarbaz]"
        "[FOOBARBAZ]"
      resultOutput r `shouldBe` (build $ do
        "  FOOBARBAZ"
        "  bar"
        "  foobar"
        "  foobarbaz"
         )

  describe "add" $ do
    it "adds an entry to an empty config" testCase $ do
      r <- pwsafe "--add http://example.com/" ""
      resultDatabase r `shouldBeBuilder` do
        "[example.com]"
        "user=default user"
        "password=default password"
        "url=http://example.com/"

    it "adds an entry to a config with one entry" testCase $ do
      r <- pwsafe "--add http://example.com/" $ build $ do
        "[foobar.com]"
        "user=foo"
        "password=bar"
      resultDatabase r `shouldBeBuilder` do
        "[foobar.com]"
        "user=foo"
        "password=bar"
        ""
        "[example.com]"
        "user=default user"
        "password=default password"
        "url=http://example.com/"

    it "adds an entry to an arbitrary config" testProperty $ \(DatabaseFile db xs) -> all ((/= "example.com") . entryName) xs ==> QC.monadicIO $ do
      r <- liftIO $ pwsafe "--add http://example.com/" db
      entry <- return . build $ do
        "[example.com]"
        "user=default user"
        "password=default password"
        "url=http://example.com/"
      let expected = if null db then entry else db ++ "\n" ++ entry
      resultDatabase r `shouldBeQC` expected

    it "fails on duplicate entry" testCase $ do
      c <- return . build $ do
        "[example.com]"
        "user=foo"
        "password=bar"
        "url=http://example.com"
      pwsafe "--add http://example.com/" c `shouldThrow` errorCall "Entry with name \"example.com\" already exists!"

    it "accepts an optional --user argument" testCase $ do
      r <- pwsafe "--add http://example.com/ --user me" ""
      resultDatabase r `shouldBeBuilder` do
        "[example.com]"
        "user=me"
        "password=default password"
        "url=http://example.com/"

  describe "query" $ do
    it "copies user name and password to clipboard" $ do
      r <- pwsafe "--query example.com" $ build $ do
        "[example.com]"
        "user=foo"
        "password=bar"
        "url=http://example.com/"
      resultClipboard r `shouldBe` ["foo", "bar"]

    it "copies password multiple times to clipboard, if -n is given" $ do
      r <- pwsafe "--query example.com -n 3" $ build $ do
        "[example.com]"
        "user=foo"
        "password=bar"
        "url=http://example.com/"
      resultClipboard r `shouldBe` ["foo", "bar", "bar", "bar"]

  describe "idCipher (test helper)" $ do
    it "can encrypt and decrypt" testProperty $ \s -> QC.monadicIO $ do
      r <- liftIO $ do
        c <- idCipher
        encrypt c s
        decrypt c
      r `shouldBeQC` s

  describe "mockSink (test helper)" $ do
    it "provides access to values put into the sink" $ do
      (sink, accessor) <- mockSink
      sink "foo"
      sink "bar"
      sink "baz"
      r <- accessor
      r `shouldBe` ["foo", "bar", "baz"]
