module Homework.Week11Spec (
  main,
  spec
) where

import Test.Hspec

import Homework.Week11.Assignment

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "week 11" $ do
    it "needs some tests!" $ do
      pending