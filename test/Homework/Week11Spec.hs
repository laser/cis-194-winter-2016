module Homework.Week11Spec (
  spec,
  forceCIToRunThisTest
) where

import Test.Hspec

import Homework.Week11.Assignment

forceCIToRunThisTest :: Bool
forceCIToRunThisTest = False

spec :: Spec
spec = do
  describe "week 11" $ do
    it "needs some tests!" $ do
      pending