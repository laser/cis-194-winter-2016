module Spec where

import Test.Hspec (hspec)

import qualified Homework.Week1Spec as Week1
import qualified Homework.Week2Spec as Week2

main :: IO ()
main = do
  hspec Week1.spec
  hspec Week2.spec
