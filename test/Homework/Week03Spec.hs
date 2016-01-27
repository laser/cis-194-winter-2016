module Homework.Week03Spec (
  main,
  spec
) where

import Test.Hspec

import Homework.Week03.Assignment

main :: IO ()
main = hspec spec

spec :: Spec
spec = do


  describe "takeEvery" $ do
    it "takes every nth character of a list" $ do
      takeEvery 1 "ABCD" `shouldBe` "ABCD"
      takeEvery 2 "ABCD" `shouldBe` "BD"
      takeEvery 3 "ABCD" `shouldBe` "C"
      takeEvery 4 "ABCD" `shouldBe` "D"
      takeEvery 5 "ABCD" `shouldBe` []

  describe "skips" $ do
    it "outputs the input list if empty" $ do
      skips ([] :: [Int]) `shouldBe` []

    it "outputs in the nth list every nth element from the input list" $ do
      skips [1] `shouldBe` [[1]]
      skips [True, False] `shouldBe` [[True, False], [False]]
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]

  describe "localMaxima" $ do
    it "returns all the local maxima in the input list, in order" $ do
      localMaxima [1, 2] `shouldBe` []
      localMaxima [1, 2, 3] `shouldBe` []
      localMaxima [2, 9, 5, 6, 1] `shouldBe` [9, 6]
      localMaxima [2, 3, 4, 1, 5] `shouldBe` [4]
      localMaxima [1, 2, 3, 4, 5] `shouldBe` []

  let hs1 = "\
             \ *        \n\
             \ *        \n\
             \ *   *    \n\
             \==========\n\
             \0123456789\n"

  describe "addBase" $ do
    it "adds the base of the graph and intercalates newlines to produce a finished answer" $ do
      addBase [" *        ", " *        ", " *   *    "] `shouldBe` hs1

  describe "addEmptyCols" $ do
    it "fills empty columns between 0-9" $ do

      let cols1 = [[],[1,1,1],[],[],[],[5],[],[],[],[]]
      addEmptyCols [0..9] [[1,1,1],[5]] `shouldBe` cols1

      let cols2 = [[0],[1],[2],[3],[4],[5],[6],[7],[8],[9]]
      addEmptyCols [0..9] cols2 `shouldBe` cols2

      -- let cols3 = [[],[],[],[],[],[],[],[],[],[]]
      -- addEmptyCols [0..9] [[]] `shouldBe` cols3

  describe "histogram" $ do
    it "takes as input a list of Integers between 0 and 9 (inclusive) and \
        \ outputs a vertical histogram showing how many of each number were in \
        \ the input list" $ do

      histogram [1, 1, 1, 5] `shouldBe` hs1

      let hs2 = "\
                 \    *     \n\
                 \    *     \n\
                 \    * *   \n\
                 \ ******  *\n\
                 \==========\n\
                 \0123456789\n"

      histogram [1, 4, 5, 4, 6, 6, 3, 4, 2, 4, 9] `shouldBe` hs2
