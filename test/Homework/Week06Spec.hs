module Homework.Week06Spec (
  spec,
  forceCIToRunThisTest
) where

import Test.Hspec

import Homework.Week06.Assignment

forceCIToRunThisTest :: Bool
forceCIToRunThisTest = False

spec :: Spec
spec = do
  describe "week 6" $ do
    it "needs some tests!" $ do
      pending