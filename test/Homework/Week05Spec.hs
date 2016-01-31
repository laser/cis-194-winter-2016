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
  describe "eval" $ do
    it "should eval an expression" $ do
      eval (Add (Lit 2) (Lit 3)) `shouldBe` 5
      eval (Mul (Lit 2) (Lit 3)) `shouldBe` 6
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

  describe "evalStr" $ do
    it "should evaluate parsed expressions" $ do
      evalStr "2+3" `shouldBe` Just 5
      evalStr "2*3" `shouldBe` Just 6
      evalStr "(2+3)*4" `shouldBe` Just 20
      evalStr "2+3 *4" `shouldBe` Just 14
      evalStr "2+3*" `shouldBe` Nothing

  describe "Expr" $ do
    it "should evaluate ExprT" $ do
      (lit 2) `shouldBe` (Lit 2)
      mul (add (lit 2) (lit 3)) (lit 4) `shouldBe` (Mul (Add (Lit 2) (Lit 3)) (Lit 4))

  describe "Expr Integer" $ do
    it "should treat (lit 2) as 2" $ do
      -- have to cast to Integer ... hmm
      (lit 2) `shouldBe` (toInteger 2)

    it "should add" $ do
      (add 2 3) `shouldBe` (toInteger 5)

    it "should add and multiply" $ do
      (add 2 (mul 2 3)) `shouldBe` (toInteger 8)

  describe "Expr Bool" $ do
    it "should treat positive numbers as True" $ do
      (lit 1) `shouldBe` True
      (lit 2) `shouldBe` True

    it "should treat <= 0 as False" $ do
      (lit   0)  `shouldBe` False
      (lit (-1)) `shouldBe` False

    it "should treat add as OR" $ do
      (add (lit 1) (lit 1)) `shouldBe` True
      (add (lit 0) (lit 1)) `shouldBe` True
      (add (lit 1) (lit 0)) `shouldBe` True
      (add (lit 0) (lit 0)) `shouldBe` False
      
    it "should treat mul as AND" $ do
      (mul (lit 1) (lit 1)) `shouldBe` True
      (mul (lit 1) (lit 0)) `shouldBe` False
      (mul (lit 0) (lit 1)) `shouldBe` False
      (mul (lit 0) (lit 0)) `shouldBe` False

    it "should evaluate nested expressions" $ do
      (add (add (lit 0)(lit 0))(mul (lit 1)(lit 2))) `shouldBe` True

  describe "Expr MinMax" $ do
    it "should treat lit as a MinMax literal" $ do
      (lit 2) `shouldBe` MinMax 2

    it "should add as 'max'" $ do
      (add (lit 0) (lit 2)) `shouldBe` MinMax 2

    it "should mul as 'min'" $ do
      (mul (lit 0) (lit 2)) `shouldBe` MinMax 0

  describe "Expr Mod7" $ do
    it "should handle literals as mod7" $ do
      (lit 7) `shouldBe` Mod7 0
      (lit 3) `shouldBe` Mod7 3
      (lit 9) `shouldBe` Mod7 2

    it "should do addition in mod7" $ do
      (add (lit 5) (lit 3)) `shouldBe` Mod7 1
      (add (lit 2) (lit 3)) `shouldBe` Mod7 5
      (add (lit 6) (lit 5)) `shouldBe` Mod7 4

    it "should do multiplication in mod7" $ do
      (mul (lit 5) (lit 5)) `shouldBe` Mod7 4
      (mul (lit 5) (lit 6)) `shouldBe` Mod7 2
      (mul (lit 2) (lit 3)) `shouldBe` Mod7 6

    it "should evaluate nested expressions" $ do
      (add (lit 1)(mul (lit 4)(lit 5))) `shouldBe` Mod7 0
