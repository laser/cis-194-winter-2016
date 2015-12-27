module Homework.Week07Spec (
  spec,
  forceCIToRunThisTest
) where

import Test.Hspec

import Homework.Week07.Assignment

forceCIToRunThisTest :: Bool
forceCIToRunThisTest = False

spec :: Spec
spec = do
  describe "week 7" $ do
    it "needs some tests!" $ do
      pending