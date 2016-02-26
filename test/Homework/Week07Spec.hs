module Homework.Week07Spec (
  main,
  spec
) where

import Test.Hspec

import Homework.Week07.Assignment

import Data.Aeson
import Data.Maybe (fromJust)
import Data.Monoid

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text                  as T

main :: IO ()
main = hspec spec

marketJSON :: String -> String -> Double -> Double -> B.ByteString
marketJSON name statename xn yn = B.pack jsonData
  where jsonData = "{   \"marketname\": \"" ++ name
                ++ "\",      \"state\": \"" ++ statename
                ++ "\",          \"x\":   " ++ show xn
                ++ "  ,          \"y\":   " ++ show yn
                ++ "}"

marketsJSON :: [(String, String, Double, Double)] -> B.ByteString
marketsJSON ms = B.concat [B.pack "[", inner, B.pack "]"]
  where jsons = map (\(n, s, xn, yn) -> marketJSON n s xn yn) ms
        inner = B.intercalate (B.pack ", ") jsons

shouldBeNamed :: Market -> String -> Expectation
shouldBeNamed market name = marketname market `shouldBe` T.pack name

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

  describe "parseMarkets" $ do
    it "returns an error for malformed JSON" $ do
      parseMarkets (B.pack "{") `shouldBe` Left "not enough input"

    it "parses JSON strings to Markets" $ do
      let markets = parseMarkets $ marketsJSON [("A", "B", 1, 2)]
      length markets `shouldBe` 1
      let (Right [market]) = markets
      marketname market `shouldBe` T.pack "A"
      state market `shouldBe` T.pack "B"
      x market `shouldBe` 1
      y market `shouldBe` 2

    it "produces one market per element in the JSON" $ do
      let (Right markets) = parseMarkets $ marketsJSON [ ("A", "B", 1, 2)
                                                       , ("C", "D", 3, 4) ]
      length markets `shouldBe` 2

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

  let searchableJSON = marketsJSON [ ("Foo",    "X",  1,  7)
                                   , ("Bar",    "X",  3, -3)
                                   , ("Baz",    "Z", -2,  3)
                                   , ("FooBar", "Z", -7,  2)
                                   ]

  describe "search" $ do
    it "returns the found markets compounded in the given monoid" $ do
      let productFound txt = getProduct . search (Product . const 2) txt
      let (Right markets) = parseMarkets searchableJSON
      productFound (T.pack "oo") markets `shouldBe` (4 :: Integer)
      productFound (T.pack "ar") markets `shouldBe` (4 :: Integer)
      productFound (T.pack "az") markets `shouldBe` (2 :: Integer)

  describe "firstFound" $ do
    it "returns the first market found" $ do
      let (Right markets) = parseMarkets searchableJSON
      fromJust (firstFound (T.pack "oo") markets) `shouldBeNamed` "Foo"
      fromJust (firstFound (T.pack "ar") markets) `shouldBeNamed` "Bar"
      fromJust (firstFound (T.pack "az") markets) `shouldBeNamed` "Baz"

  describe "lastFound" $ do
    it "returns the last market found" $ do
      let (Right markets) = parseMarkets searchableJSON
      fromJust (lastFound (T.pack "oo") markets) `shouldBeNamed` "FooBar"
      fromJust (lastFound (T.pack "ar") markets) `shouldBeNamed` "FooBar"
      fromJust (lastFound (T.pack "az") markets) `shouldBeNamed` "Baz"

  describe "allFound" $ do
    it "returns all markets found" $ do
      let (Right markets) = parseMarkets searchableJSON
      length (allFound (T.pack "oo") markets) `shouldBe` 2
      length (allFound (T.pack "ar") markets) `shouldBe` 2
      length (allFound (T.pack "az") markets) `shouldBe` 1

  describe "numberFound" $ do
    it "returns the number of markets found" $ do
      let (Right markets) = parseMarkets searchableJSON
      numberFound (T.pack "oo") markets `shouldBe` 2
      numberFound (T.pack "ar") markets `shouldBe` 2
      numberFound (T.pack "az") markets `shouldBe` 1

  describe "orderedNtoS" $ do
    it "returns the markets found, sorted from north to south" $ do
      let (Right markets) = parseMarkets searchableJSON

      let [a, b] = orderedNtoS (T.pack "oo") markets
      a `shouldBeNamed` "FooBar"
      b `shouldBeNamed` "Foo"

      let [c, d, e] = orderedNtoS (T.pack "B") markets
      c `shouldBeNamed` "Bar"
      d `shouldBeNamed` "FooBar"
      e `shouldBeNamed` "Baz"
