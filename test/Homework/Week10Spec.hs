module Homework.Week10Spec (
  spec,
  forceCIToRunThisTest
) where

import Test.Hspec

import Homework.Week10.Assignment

forceCIToRunThisTest :: Bool
forceCIToRunThisTest = False

spec :: Spec
spec = do
  describe "week 10" $ do
    it "needs some tests!" $ do
      pending