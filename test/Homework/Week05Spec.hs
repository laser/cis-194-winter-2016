module Homework.Week05Spec (
  spec,
  forceCIToRunThisTest
) where

import Test.Hspec

import Homework.Week05.Assignment

forceCIToRunThisTest :: Bool
forceCIToRunThisTest = False

spec :: Spec
spec = do
  describe "week 5" $ do
    it "needs some tests!" $ do
      pending