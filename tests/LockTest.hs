{-# LANGUAGE TemplateHaskell #-}
module LockTest (tests) where
import           Test.Framework
import           Test.Framework.TH
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit hiding (assert, (@=?))
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Control.Monad (when)
import           Control.Exception (finally)

import qualified Lock

main  = defaultMain [tests]
tests = $(testGroupGenerator)

-- | Make sure that lock is not currently held, run action, release lock
--
-- Every test should be wrapped with this!
lockTest :: IO a -> IO a
lockTest action = do
  r <- Lock.test
  when r $ error "The lock is currently held! Are you accessing the resource right now?"
  action `finally` Lock.release

case_test = lockTest $ do
  False @=? Lock.test
  Lock.acquire
  True  @=? Lock.test
  Lock.release
  False @=? Lock.test
  where
    (@=?) :: (Show a, Eq a) => a -> IO a -> Assertion
    expected @=? action = do
      actual <- action
      assertEqual "" expected actual

data Action = Acquire | Release
  deriving (Eq, Show)

instance Arbitrary Action where
  arbitrary = (\x -> if x then Acquire else Release) `fmap` arbitrary

prop_acquireRelease :: [Action] -> Property
prop_acquireRelease actions = monadicIO $ do
  result <- run $ lockTest $ mapM runAction actions
  assert $ result == map snd (deduceResult actions)
  return ()
  where
    runAction Acquire = Lock.acquire
    runAction Release = Lock.release

    deduceResult :: [Action] -> [(Action, Bool)]
    deduceResult = reverse . foldr step [] . reverse
      where
        step currentAction []                          = [(currentAction, currentAction == Acquire)]
        step currentAction l@((previousAction, _) : _) =  (currentAction, currentAction /= previousAction ) : l

