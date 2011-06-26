{-# LANGUAGE TemplateHaskell #-}
module Main where
import Test.Framework.TH (defaultMainGenerator)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Test.HUnit

import qualified Options
import           Options (Mode(..))

main = $(defaultMainGenerator)

case_getOptions = do
  opts <- Options.get ["--query", "foobar"]
  Query "foobar" @=? Options.mode opts
