module Homework.Week05Spec (
  main,
  spec
) where

import Test.Hspec

import Homework.Week05.Assignment

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "week 5" $ do
    it "needs some tests!" $ do
      pending