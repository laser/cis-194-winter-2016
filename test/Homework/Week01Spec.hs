module Homework.Week01Spec (
  main,
  spec
) where

import Test.Hspec

import Homework.Week01.Assignment

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "toDigits" $ do
    it "should split digits of integer into a list" $ do
      pending
      toDigits 1234 `shouldBe` [1,2,3,4]

    it "should return an empty list for zero" $ do
      pending
      toDigits 0 `shouldBe` []

    it "should return an empty list for negative numbers" $ do
      pending
      toDigits (-1) `shouldBe` []

  describe "toDigitsRev" $ do
    it "should return an empty list for zero" $ do
      pending
      toDigitsRev 0 `shouldBe` []

    it "should return an empty list for negative numbers" $ do
      pending
      toDigitsRev (-1) `shouldBe` []
      toDigitsRev (-22222) `shouldBe` []

    it "should split digits of integer into a list in reverse order" $ do
      pending
      toDigitsRev 123 `shouldBe` [3,2,1]
      toDigitsRev 431 `shouldBe` [1,3,4]
      toDigitsRev 12 `shouldBe` [2,1]
      toDigitsRev 2 `shouldBe` [2]

  describe "doubleEveryOther" $ do
    it "should return an empty list given an empty list" $ do
      pending
      doubleEveryOther [] `shouldBe` []

    it "should double every other int in the list, from right to left" $ do
      pending
      doubleEveryOther [8,7,6,5] `shouldBe`[16,7,12,5]
      doubleEveryOther [1,2,3] `shouldBe` [1,4,3]

  describe "sumDigits" $ do
    it "should return zero for an empty list" $ do
      pending
      sumDigits [] `shouldBe` 0

    it "should sum all digits in the list" $ do
      pending
      sumDigits [16,7,12,5] `shouldBe` 22
      sumDigits [18,7,33,5] `shouldBe` 27

  describe "validate" $ do
    it "should return True for valid card number" $ do
      pending
      validate 4012888888881881 `shouldBe` True

    it "should return False for invalid card number" $ do
      pending
      validate 4012888888881882 `shouldBe` False

  describe "hanoi" $ do
    it "should return an empty list for zero discs" $ do
      pending
      hanoi 0 "a" "b" "c" `shouldBe` []

    it "should solve for 1 disc" $ do
      pending
      hanoi 1 "a" "b" "c" `shouldBe` [("a", "b")]

    it "should take 32767 moves to move 15 discs" $ do
      length (hanoi 15 "a" "b" "c") `shouldBe` 32767

  describe "hanoi4" $ do
    it "should return an empty list for zero discs" $ do
      hanoi4 0 "a" "b" "c" "d" `shouldBe` []

    --it "should take 129 moves to move 15 discs" $ do
    --  length (hanoi4 15 "a" "b" "c" "d") `shouldBe` 129
