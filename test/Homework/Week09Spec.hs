module Homework.Week09Spec (
  main,
  spec
) where

import Test.Hspec

import Homework.Week09.Assignment

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "week 09" $ do
    it "needs some tests!" $ do
      pending