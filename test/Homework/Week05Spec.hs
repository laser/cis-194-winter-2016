module Homework.Week05Spec (
  main,
  spec
) where

import Test.Hspec

import Homework.Week05.Assignment

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "week 5" $ do
    it "should eval an expression" $ do
      eval (Add (Lit 2) (Lit 3)) `shouldBe` 5
      eval (Mul (Lit 2) (Lit 3)) `shouldBe` 6
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

    it "should evaluate parsed expressions" $ do
      evalStr "2+3" `shouldBe` Just 5
      evalStr "2*3" `shouldBe` Just 6
      evalStr "(2+3)*4" `shouldBe` Just 20
      evalStr "2+3 *4" `shouldBe` Just 14
      evalStr "2+3*" `shouldBe` Nothing

    it "should evaluate Expr to ExprT" $ do
      (lit 2) `shouldBe` (Lit 2)
      mul (add (lit 2) (lit 3)) (lit 4) `shouldBe` (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
