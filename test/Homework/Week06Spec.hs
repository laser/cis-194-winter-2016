module Homework.Week06Spec (
  main,
  spec
) where

import Test.Hspec

import Homework.Week06.Assignment

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fib" $ do
    it "should calculate the nth fibonacci number - naive version" $ do
      fib 0 `shouldBe` 0
      fib 1 `shouldBe` 1
      fib 2 `shouldBe` 1
      fib 14 `shouldBe` 377 -- 14 is the 15th (zero-indexed)

  describe "fibs1" $ do
    it "should calculate an infinte fibonacci sequence - slowly" $ do
      last (take 15 fibs1) `shouldBe` 377
      last (take 35 fibs1) `shouldBe` 5702887

  describe "fibs2" $ do
    it "should calculate an infinte fibonacci sequence - efficiently" $ do
      last (take 35 fibs2) `shouldBe` 5702887
