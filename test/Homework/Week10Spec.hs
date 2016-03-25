module Homework.Week10Spec (
  main,
  spec
) where

import Test.Hspec

import Homework.Week10.Assignment

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "week 10" $ do
    it "needs some tests!" $ do
      shouldBe True True
