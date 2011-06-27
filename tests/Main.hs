{-# LANGUAGE TemplateHaskell #-}
module Main where
import Test.Framework
import Test.Framework.TH (testGroupGenerator)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Test.HUnit

import qualified Options
import           Options (Mode(..))

import qualified LockTest

main = defaultMain [tests, LockTest.tests]

tests = $(testGroupGenerator)

case_getOptions_query = do
  opts <- Options.get ["--query", "foobar"]
  Query "foobar" @=? Options.mode opts

case_getOptions_dbfile = do

  opts <- Options.get []
  db <- Options.defaultDatabaseFile
  db @=? Options.databaseFile opts

  opts <- Options.get ["--dbfile", "mydb"]
  "mydb" @=? Options.databaseFile opts

case_getOptions_precedence = do
  opts <- Options.get ["--query", "foo", "--help", "--add", "baz"]
  Add "baz" @=? Options.mode opts

  opts <- Options.get ["--add", "foo", "--help", "--query", "baz"]
  Query "baz" @=? Options.mode opts
