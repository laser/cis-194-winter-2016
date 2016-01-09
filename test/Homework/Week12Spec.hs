module Homework.Week12Spec (
  main,
  spec
) where

import Test.Hspec

import Homework.Week12.Assignment

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "week 12" $ do
    it "needs some tests!" $ do
      pending