module Homework.Week09Spec (
  main,
  spec
) where

import Test.Hspec
import Test.QuickCheck

import Homework.Week09.AParser
import Homework.Week09.Assignment

import Control.Applicative
import Data.Char (isNumber, isUpper)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Functor Parser" $ do
    describe "fmap" $ do
      it "applies a function to the result of a parser" $ do
        let p = fmap (+1) posInt
        runParser p "41" `shouldBe` Just (42, "")
        runParser p "x"  `shouldBe` Nothing

  describe "Applicative Parser" $ do
    describe "pure" $ do
      it "creates a parser that consumes nothing and returns a value" $
        property $ \str -> runParser (pure ()) str == Just ((), str)

    describe "<*>" $ do
      it "applies a function from a parser to the result of a parser" $ do
        let p1 = pure (+1) <*> posInt
        runParser p1 "41" `shouldBe` Just (42, "")

        let p2 = (+) <$> posInt <* char ' ' <*> posInt
        runParser p2 "12 13a" `shouldBe` Just (25, "a")

  describe "abParser" $ do
    it "parses the characters 'a' and 'b' as a pair" $ do
      runParser abParser "abcdef" `shouldBe` Just (('a', 'b'), "cdef")
      runParser abParser "bcdefa" `shouldBe` Nothing
      runParser abParser "aecdbf" `shouldBe` Nothing

  describe "abParser_" $ do
    it "parses the characters 'a' and 'b' but returns nothing" $ do
      runParser abParser_ "abcdef" `shouldBe` Just ((), "cdef")
      runParser abParser_ "bcdefa" `shouldBe` Nothing
      runParser abParser_ "aecdbf" `shouldBe` Nothing

  describe "intPair" $ do
    it "parses two integer values separated by a space" $ do
      runParser intPair "12 34" `shouldBe` Just ([12, 34], "")

  describe "Alternative Parser" $ do
    describe "empty" $ do
      it "is a parser that always fails" $ do
        runParser (empty :: Parser ()) "abc" `shouldBe` Nothing

    describe "<|>" $ do
      it "uses the first parser if successful" $ do
        let p1 = char '*' <|> char '$'
        runParser p1 "*abc" `shouldBe` Just ('*', "abc")

        let p2 = satisfy isNumber <|> (head . show <$> posInt)
        runParser p2 "1234" `shouldBe` Just ('1', "234")

      it "user the second parser if the first one fails" $ do
        let p1 = char '*' <|> char '$'
        runParser p1 "$abc" `shouldBe` Just ('$', "abc")

        let p2 = satisfy isNumber <|> pure '*'
        runParser p2 "abcd" `shouldBe` Just ('*', "abcd")

        let p3 = satisfy isUpper <|> satisfy isNumber
        runParser p3 "1234" `shouldBe` Just ('1', "234")
        runParser p3 "*234" `shouldBe` Nothing

  describe "intOrUppercase" $ do
    it "consumes an integer or an uppercase character" $ do
      runParser intOrUppercase "342abcd" `shouldBe` Just ((), "abcd")
      runParser intOrUppercase "XYZ" `shouldBe` Just ((), "YZ")
      runParser intOrUppercase "foo" `shouldBe` Nothing

  let upper = satisfy isUpper

  describe "zeroOrMore" $ do
    it "runs the parser zero or more times" $ do
      runParser (zeroOrMore upper) "ABCdEfgH" `shouldBe` Just ("ABC", "dEfgH")
      runParser (zeroOrMore upper) "abcdeFGh" `shouldBe` Just ("", "abcdeFGh")

  describe "oneOrMore" $ do
    it "runs the parser one or more times" $ do
      runParser (oneOrMore upper) "ABCdEfgH" `shouldBe` Just ("ABC", "dEfgH")
      runParser (oneOrMore upper) "abcdeFGh" `shouldBe` Nothing

  describe "spaces" $ do
    it "parses a consecutive list of zero or more whitespace characters" $ do
      runParser spaces "   a b c " `shouldBe` Just ("   ", "a b c ")
      runParser spaces " \n \r \t a b c " `shouldBe` Just (" \n \r \t ", "a b c ")
      runParser spaces "a b c " `shouldBe` Just ("", "a b c ")
      runParser spaces "" `shouldBe` Just ("", "")

  describe "ident" $ do
    it "parses an alphabetic char followed by zero or more alphanumerics" $ do
      runParser ident "foobar baz" `shouldBe` Just ("foobar", " baz")
      runParser ident "foo33fA" `shouldBe` Just ("foo33fA", "")
      runParser ident "2bad" `shouldBe` Nothing
      runParser ident "" `shouldBe` Nothing

  describe "parseSExpr" $ do
    it "parses numeric atoms" $ do
      runParser parseSExpr "5" `shouldBe` Just (A (N 5), "")

    it "parses identifiers" $ do
      runParser parseSExpr "foo3" `shouldBe` Just (A (I "foo3"), "")

    it "parses lists of s-expressions" $ do
      runParser parseSExpr "(bar (foo) 3 5 874)"
        `shouldBe` Just (Comb [ A (I "bar"), Comb [A (I "foo")]
                              , A (N 3), A (N 5), A (N 874)
                              ], "")

      runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)"
        `shouldBe` Just (Comb [ Comb [ Comb [ A (I "lambda"), A (I "x")
                                     , Comb [ A (I "lambda"), A (I "y")
                              , Comb [ A (I "plus"), A (I "x"), A (I "y") ]]]
                              , A (N 3) ], A (N 5) ], "")
