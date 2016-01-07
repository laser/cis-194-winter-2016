module Homework.Week07Spec (
  main,
  spec
) where

import Test.Hspec

import Homework.Week07.Assignment

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "week 7" $ do
    it "needs some tests!" $ do
      pending