module Homework.Week10Spec (
  main,
  spec
) where

import Test.Hspec

import Control.Monad
import Control.Monad.Random

import Homework.Week10.Assignment

main :: IO ()
main = hspec spec

shouldBeWithin :: (Ord a, Show a) => a -> (a, a) -> Expectation
n `shouldBeWithin` (low, high) =
  when (n < low || n > high) $
    expectationFailure $
      unlines [ "expected: a value in the range [" ++ show low ++ ", " ++ show high ++ "]"
              , " but got: " ++ show n ]

shouldBeRoughly :: (Num a, Ord a, Show a) => a -> (a, a) -> Expectation
n `shouldBeRoughly` (target, wiggle) =
  when (n < (target - wiggle) || n > (target + wiggle)) $
    expectationFailure $
      unlines [ "expected: " ++ show target ++ "±" ++ show wiggle
              , " but got: " ++ show n ]

fuzz :: (Real b) => (a -> b) -> Int -> Rand StdGen a -> IO Double
fuzz f times rand = do
  vals <- evalRandIO $ replicateM times rand
  let nums = map (realToFrac . f) vals
  let total = sum nums
  return (total / realToFrac times)

spec :: Spec
spec = do
  describe "battle" $ do
    it "simulates a single Risk battle" $ do
      attack <- fuzz attackers 1000 (battle $ Battlefield 10 20)
      attack `shouldBeRoughly` (8.5, 0.1)

      defense <- fuzz defenders 1000 (battle $ Battlefield 10 20)
      defense `shouldBeRoughly` (19.5, 0.1)

  describe "invade" $ do
    it "simulates a repeated Risk battle" $ do
      attack <- fuzz attackers 1000 (invade $ Battlefield 10 20)
      attack `shouldBeRoughly` (1.0, 0.5)
