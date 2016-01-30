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

  describe "streamRepeat" $ do
    it "should produce a stream containing infinitely many copies of the same element" $ do
      take 50 (streamToList (streamRepeat 1)) `shouldBe` (replicate 50 1)

  describe "streamMap" $ do
    it "should apply a function to every element of a stream" $ do
      take 10 (streamToList (streamMap (*3) (streamRepeat 2))) `shouldBe` (replicate 10 6)

  describe "streamFromSeed" $ do
    it "should use the given function to generate the next element of a stream from a given element" $ do
      take 10 (streamToList (streamFromSeed (*2) 2)) `shouldBe` [2,4,8,16,32,64,128,256,512,1024]

  describe "nats" $ do
    it "should be all natural numbers" $ do
      take 20 (streamToList nats) `shouldBe` [0..19]

  describe "interleaveStreams" $ do
    it "should alternate values between two streams" $ do
      take 6 (streamToList (interleaveStreams (streamRepeat 0) (streamRepeat 1))) `shouldBe` [0,1,0,1,0,1]

  describe "ruler" $ do
    it "should calc the largest power of 2 that evenly divides into each element (starting at 1)" $ do
      take 32 (streamToList ruler) `shouldBe` [0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,5]
      
