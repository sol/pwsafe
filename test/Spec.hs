module Spec (main) where

import           Test.Spec

import           Options (Mode(..))
import qualified Options

import qualified LockTest
import qualified DatabaseSpec

main :: IO ()
main = run $ do

  describe "Options.get" $ do
    it "recognizes --query" $ do
      opts <- Options.get ["--query", "foobar"]
      Options.mode opts `shouldBe` Query "foobar"

    it "recognizes --dbfile" $ do
      opts <- Options.get ["--dbfile", "mydb"]
      Options.databaseFile opts `shouldBe` "mydb"

    it "returns default database file if --dbfile is not given" $ do
      dbfile <- Options.defaultDatabaseFile
      opts <- Options.get []
      Options.databaseFile opts `shouldBe` dbfile 

    it "gives precedence to the last mode of operation (1)" $ do
      opts <- Options.get ["--query", "foo", "--help", "--add", "baz"]
      Options.mode opts `shouldBe` Add "baz" 

    it "gives precedence to the last mode of operation (2)" $ do
      opts <- Options.get ["--add", "foo", "--help", "--query", "baz"]
      Options.mode opts `shouldBe` Query "baz"

  describe "Lock" $ do
    add LockTest.test

  describe "Database" DatabaseSpec.spec
