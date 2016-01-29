module Homework.Week07Spec (
  main,
  spec
) where

import Test.Hspec

import Homework.Week07.Assignment

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ynToBool" $ do
    it "converts Y strings to True" $ do
      ynToBool (toJSON "Y") `shouldBe` toJSON True

    it "converts N strings to False" $ do
      ynToBool (toJSON "N") `shouldBe` toJSON False

    it "converts values nested within Arrays" $ do
      ynToBool (toJSON ["Y", "N"]) `shouldBe` toJSON [True, False]

    it "converts values nested within Objects" $ do
      ynToBool (toJSON [("a", "Y"), ("b", "N")])
        `shouldBe` toJSON [("a", True), ("b", False)]

    it "lets other values pass through" $ do
      ynToBool (toJSON "something") `shouldBe` toJSON "something"
      ynToBool (toJSON True) `shouldBe` toJSON True
      ynToBool (toJSON False) `shouldBe` toJSON False
      ynToBool (toJSON (42 :: Double)) `shouldBe` toJSON (42 :: Double)
      ynToBool (toJSON ["A", "B", "C"]) `shouldBe` toJSON ["A", "B", "C"]

    it "handles deeply nested objects" $ do
      {- Sorry for the mess: Haskell has no heredocs!
         The input JSON looks like this:

         {
           "a": [{}, {"x": "Y"}],
           "b": [{"y": 42}, {"z": ["A", "N"]}],
         }

         The "Y" and "N" should be replaced as expected. -}
      let (Just input)  = decode' $ B.pack ("[{\"a\": [{}, {\"x\": \"Y\"}]}," ++
                                            " {\"b\": [{\"y\": 42}," ++
                                            " {\"z\": [\"A\", \"N\"]}]}]")
      let (Just output) = decode' $ B.pack ("[{\"a\": [{}, {\"x\": true}]}," ++
                                            " {\"b\": [{\"y\": 42}," ++
                                            " {\"z\": [\"A\", false]}]}]")
      ynToBool input `shouldBe` output

  describe "parseData" $ do
    it "returns an error for malformed JSON" $ do
      parseData (B.pack "{") `shouldBe` Left "not enough input"

    it "parses JSON strings, replacing Y/N with booleans" $ do
      parseData (B.pack "\"Y\"") `shouldBe` Right (toJSON True)
      parseData (B.pack "\"N\"") `shouldBe` Right (toJSON False)
      parseData (B.pack "[1, 2, 3]")
        `shouldBe` Right (toJSON ([1, 2, 3] :: [Double]))
      parseData (B.pack "[\"N\", \"Y\", \"N\"]")
        `shouldBe` Right (toJSON [False, True, False])

  describe "Monoid OrdList" $ do
    describe "mempty" $ do
      it "is the empty list" $ do
        mempty `shouldBe` OrdList ([] :: [Integer])

    describe "mappend" $ do
      it "preserves order in the result" $ do
        (OrdList [1, 2, 3] `mappend` OrdList [4, 5, 6])
          `shouldBe` OrdList ([1, 2, 3, 4, 5, 6] :: [Integer])
        (OrdList [4, 5, 6] `mappend` OrdList [1, 2, 3])
          `shouldBe` OrdList ([1, 2, 3, 4, 5, 6] :: [Integer])
        (OrdList [1, 3, 6] `mappend` OrdList [2, 4, 5])
          `shouldBe` OrdList ([1, 2, 3, 4, 5, 6] :: [Integer])
        (OrdList [2, 4, 5] `mappend` OrdList [1, 3, 6])
          `shouldBe` OrdList ([1, 2, 3, 4, 5, 6] :: [Integer])
        (OrdList [1, 2, 3] `mappend` OrdList [1, 2, 3])
          `shouldBe` OrdList ([1, 1, 2, 2, 3, 3] :: [Integer])
