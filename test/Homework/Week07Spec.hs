module Homework.Week07Spec (
  main,
  spec
) where

import           Test.Hspec

import           Homework.Week07.Assignment

import           Data.Aeson
import           Data.Maybe                 (fromJust)
import           Data.Monoid

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
  describe "cheating" $ do
    it "is wrong!" $ do
      1 `shouldBe` 1
