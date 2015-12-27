module Homework.Week09Spec (
  spec,
  forceCIToRunThisTest
) where

import Test.Hspec

import Homework.Week09.Assignment

forceCIToRunThisTest :: Bool
forceCIToRunThisTest = False

spec :: Spec
spec = do
  describe "week 9" $ do
    it "needs some tests!" $ do
      pending