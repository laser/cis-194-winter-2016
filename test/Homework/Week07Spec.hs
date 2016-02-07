module Homework.Week07Spec (
  main,
  spec
) where

import Test.Hspec

import Homework.Week07.Assignment
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import Data.HashMap.Strict as H
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

    it "should leave all other Strings unchanged" $ do
      let no = (String $ T.pack "No")
      ynToBool no `shouldBe`  no

    it "change all instances of Y/N to Bool in an Object" $ do
      let foo = T.pack "foo"
      let bar = T.pack "bar"
      let baz = T.pack "baz"

      let obj = fromList [(foo, (String $ T.pack "Y"))
                         ,(bar, (String $ T.pack "N"))
                         ,(baz, Number 1)]

      let expected = fromList [(foo, Bool True)
                              ,(bar, Bool False)
                              ,(baz, Number 1)]

      ynToBool (Object obj) `shouldBe` Object expected

  describe "parseData" $ do
    it "should parse a JSON bytestring" $ do
      let foo = parseData $ B.pack "{ \"foo\":1,\"bar\":\"Y\"}"
      foo `shouldBe` Right (Object (fromList [(T.pack "foo", Number 1)
                                             ,(T.pack "bar", Bool True)]))

    it "should return an error if parsing fails" $ do
      let foo = parseData $ B.pack "{ xxx }"
      foo `shouldBe` Left ("Failed reading: satisfy") -- whatever that means


