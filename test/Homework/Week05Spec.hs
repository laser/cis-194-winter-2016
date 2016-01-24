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

    it "should treat Integers as instances of Expr" $ do
      -- have to cast to Integer ... hmm
      (lit 2) `shouldBe` (toInteger 2)
      (add 2 3) `shouldBe` (toInteger 5)
      (add 2 (mul 2 3)) `shouldBe` (toInteger 8)

    it "should treat Bools as instances of Expr" $ do
      (lit 2) `shouldBe` True
      (lit 1) `shouldBe` True
      (lit 0) `shouldBe` False
      (lit (-1)) `shouldBe` False
      (add (lit 0) (lit 2)) `shouldBe` True
      (add (lit 0) (lit 0)) `shouldBe` False
      (mul (lit 1) (lit 2)) `shouldBe` True
      (mul (lit 1) (lit 0)) `shouldBe` False
      (add (add (lit 0)(lit 0))(mul (lit 1)(lit 2))) `shouldBe` True

    it "should treat MinMax as instances of Expr" $ do
      (lit 2) `shouldBe` MinMax 2
      (add (lit 0) (lit 2)) `shouldBe` MinMax 2
      (mul (lit 0) (lit 2)) `shouldBe` MinMax 0

    it "should treat Mod7 as instances of Expr" $ do
      (lit 7) `shouldBe` Mod7 0
      (lit 3) `shouldBe` Mod7 3
      (lit 9) `shouldBe` Mod7 2
      (add (lit 5) (lit 3)) `shouldBe` Mod7 1
      (mul (lit 5) (lit 4)) `shouldBe` Mod7 6
      (add (lit 1)(mul (lit 4)(lit 5))) `shouldBe` Mod7 0
