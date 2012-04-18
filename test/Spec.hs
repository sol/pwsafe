module Spec (main) where

import           Test.Hspec.ShouldBe

import qualified OptionsSpec
import qualified LockSpec
import qualified DatabaseSpec
import qualified ActionSpec

main :: IO ()
main = hspecX $ do
  describe "Lock"     LockSpec.spec
  describe "Options"  OptionsSpec.spec
  describe "Database" DatabaseSpec.spec
  describe "Action"   ActionSpec.spec
