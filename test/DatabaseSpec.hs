{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-missing-signatures -fno-warn-orphans #-}
module DatabaseSpec (main, spec, DatabaseFile(..)) where

import           Test.Hspec.ShouldBe
import           Test.QuickCheck
import           Data.List
import           Control.Applicative hiding (empty)
import           Data.String.Builder
import           Database hiding (addEntry)
import qualified Database

instance Arbitrary Entry where
  arbitrary = do
    name <- shortWord
    user <- genMaybe shortWord
    password <- shortWord
    url <- genMaybe shortWord
    return $ Entry name user password url
    where
      shortWord = vectorOf 3 $ choose ('a', 'z')

      genMaybe :: Gen a -> Gen (Maybe a)
      genMaybe a = oneof [return Nothing, Just <$> a]

data DatabaseFile = DatabaseFile String [Entry]
  deriving Show

dbFromList xs = DatabaseFile (render $ foldr addEntry empty xs) xs

instance Arbitrary DatabaseFile where
  arbitrary = do
    xs <- nubBy (\a b -> entryName a == entryName b) <$> (resize 20 $ listOf arbitrary)
    return $ dbFromList xs

  shrink (DatabaseFile _ []) = []
  shrink (DatabaseFile _ xs) = [ dbFromList (xs \\ [x]) | x <- xs ]

addEntry :: Entry -> Database -> Database
addEntry e db = either error id $ Database.addEntry db e

entry name user password url = Database.Entry name (Just user) password (Just url)

(-:) :: a -> (a -> b) -> b
x -: f = f x

shouldRenderTo db builder = render db `shouldBe` build builder

main = hspec spec

spec = do
  describe "addEntry" $ do

    it "adds an entry to an empty database" $ do
      addEntry (entry "example.com" "foo" "bar" "http://example.com") empty `shouldRenderTo` do
        "[example.com]"
        "user=foo"
        "password=bar"
        "url=http://example.com"

    it "adds an entry to a database with one entry" $ do
      parse . build $ do
        "[foobar]"
        "user=one"
        "password=two"
        "url=three"
      -: addEntry (entry "example.com" "foo" "bar" "http://example.com") `shouldRenderTo` do
        "[foobar]"
        "user=one"
        "password=two"
        "url=three"
        ""
        "[example.com]"
        "user=foo"
        "password=bar"
        "url=http://example.com"

    it "adds an entry to an arbitrary database" $ property $ \(DatabaseFile input _) e ->
      let
        name = entryName e
        db = parse input
        renderedDb = let s = render db in if null s then "" else s ++ "\n"
      in
        (not $ hasEntry name db) ==> (renderedDb ++) (render $ addEntry e empty) == (render $ addEntry e db)

    it "accepts Nothing for user" $ do
      addEntry (Entry "example.com" Nothing "bar" (Just "http://example.com")) empty `shouldRenderTo` do
        "[example.com]"
        "password=bar"
        "url=http://example.com"

    it "accepts Nothing for url" $ do
      addEntry (Entry "example.com" (Just "foo") "bar" Nothing) empty `shouldRenderTo` do
        "[example.com]"
        "user=foo"
        "password=bar"

  describe "lookupEntry" $ do
    it "works on a database with one entry" $ do
      db <- return . parse . build $ do
        "[example.com]"
        "user=foo"
        "password=bar"
        "url=http://example.com"
      lookupEntry db "example.com" `shouldBe` Right (entry "example.com" "foo" "bar" "http://example.com")

    it "works on a database with arbitrary entries" $ property $ \(DatabaseFile input xs) ->
      (not . null) xs ==> do
        x <- elements xs
        return $ lookupEntry (parse input) (entryName x) == Right x
