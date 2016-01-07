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
  describe "week 6" $ do
    it "needs some tests!" $ do
      pending