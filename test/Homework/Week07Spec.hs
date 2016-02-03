module Homework.Week07Spec (
  main,
  spec
) where

import Test.Hspec

import Homework.Week07.Assignment
import qualified Data.Text as T
import Data.Aeson

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ynToBool" $ do
    it "should convert 'Y' to True" $ do
      ynToBool (String $ T.pack "Y") `shouldBe`  Bool True

    it "should convert 'N' to False" $ do
      ynToBool (String $ T.pack "N") `shouldBe`  Bool False

    it "should leave all other values unchanged" $ do
      let no = (String $ T.pack "No")
      let yo = (String $ T.pack "Yo")
      ynToBool no `shouldBe`  no
      ynToBool yo `shouldBe`  yo

      ynToBool Null `shouldBe` Null
