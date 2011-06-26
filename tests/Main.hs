{-# LANGUAGE TemplateHaskell #-}
module Main where
import Test.Framework.TH (defaultMainGenerator)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Test.HUnit

import qualified Options
import           Options (Mode(..))

main = $(defaultMainGenerator)

case_getOptions_query = do
  opts <- Options.get ["--query", "foobar"]
  Query "foobar" @=? Options.mode opts

case_getOptions_dbfile = do

  opts <- Options.get []
  db <- Options.defaultDatabaseFile
  db @=? Options.databaseFile opts

  opts <- Options.get ["--dbfile", "mydb"]
  "mydb" @=? Options.databaseFile opts
