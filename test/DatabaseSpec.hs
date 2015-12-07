{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module DatabaseSpec (main, spec, DatabaseFile(..)) where

import           Test.Hspec
import           Test.QuickCheck
import           Data.List
import           Data.String.Builder
import           Database hiding (addEntry)
import qualified Database

instance Arbitrary Entry where
  arbitrary = do
    name <- shortWord
    user <- genMaybe shortWord
    password <- shortWord
    url <- genMaybe shortWord
    return $ Entry name user (Just password) url
    where
      shortWord = vectorOf 3 $ choose ('a', 'z')

      genMaybe :: Gen a -> Gen (Maybe a)
      genMaybe a = oneof [return Nothing, Just <$> a]

data DatabaseFile = DatabaseFile String [Entry]
  deriving Show

dbFromList :: [Entry] -> DatabaseFile
dbFromList xs = DatabaseFile (render $ foldr addEntry empty xs) xs

instance Arbitrary DatabaseFile where
  arbitrary = do
    xs <- nubBy (\a b -> entryName a == entryName b) <$> (resize 20 $ listOf arbitrary)
    return $ dbFromList xs

  shrink (DatabaseFile _ []) = []
  shrink (DatabaseFile _ xs) = [ dbFromList (xs \\ [x]) | x <- xs ]

addEntry :: Entry -> Database -> Database
addEntry e db = either error id $ Database.addEntry db e

entry :: String -> String -> String -> String -> Entry
entry name user password url = Database.Entry name (Just user) (Just password) (Just url)

(-:) :: a -> (a -> b) -> b
x -: f = f x

shouldRenderTo :: Database -> Builder -> Expectation
shouldRenderTo db builder = render db `shouldBe` build builder

main :: IO ()
main = hspec spec

spec :: Spec
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
      addEntry (Entry "example.com" Nothing (Just "bar") (Just "http://example.com")) empty `shouldRenderTo` do
        "[example.com]"
        "password=bar"
        "url=http://example.com"

    it "accepts Nothing for password" $ do
      addEntry (Entry "example.com" (Just "foo") Nothing (Just "http://example.com")) empty `shouldRenderTo` do
        "[example.com]"
        "user=foo"
        "url=http://example.com"

    it "accepts Nothing for url" $ do
      addEntry (Entry "example.com" (Just "foo") (Just "bar") Nothing) empty `shouldRenderTo` do
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

    it "works on a database with arbitrary entries" $
      property $ \(DatabaseFile input xs) ->
        (not . null) xs ==> do
          x <- elements xs
          return $ lookupEntry (parse input) (entryName x) == Right x
