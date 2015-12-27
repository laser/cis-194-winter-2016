module Homework.Week12Spec (
  spec,
  forceCIToRunThisTest
) where

import Test.Hspec

import Homework.Week12.Assignment

forceCIToRunThisTest :: Bool
forceCIToRunThisTest = False

spec :: Spec
spec = do
  describe "week 12" $ do
    it "needs some tests!" $ do
      pending