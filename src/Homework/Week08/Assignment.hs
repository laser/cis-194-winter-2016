module Homework.Week08.Assignment (
  first,
  abParser,
  abParser_,
  intPair,
  intOrUppercase
) where

import Homework.Week08.AParser
import Control.Applicative
import Data.Maybe
import Data.Char

-- #1
first :: (a -> b) -> (a,c) -> (b,c)
first f (x, y) = (f x, y)

-- newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap g (Parser f)  = Parser $ \str -> fmap (first g) (f str)

-- #2
instance Applicative Parser where
  pure a = Parser (\str -> Just (a, str))

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  -- (<*>) :: Parser { runParser :: String -> Maybe (a->b, String) } -> Parser { runParser :: String -> Maybe (a, String) } -> Parser { runParser :: String -> Maybe (b, String) }
  p1 <*> p2 = Parser $ \str ->
    case runParser p1 str of
      Nothing -> Nothing
      Just (p1Func, p1Str) -> runParser (fmap p1Func p2) p1Str

-- #3
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'


abParser_ :: Parser ()
abParser_ = (\a b -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\a b c -> [a, c]) <$> posInt <*> char ' ' <*> posInt

-- #4
instance Alternative Parser where
  empty = Parser (\s -> Nothing)
  p1 <|> p2 = Parser $ \str ->
    case runParser p1 str of
      Nothing -> runParser p2 str
      Just (p1Func, p1Str) -> Just (p1Func, p1Str)

-- #5
intOrUppercase :: Parser ()
intOrUppercase = (\a -> ()) <$> posInt <|> (\a -> ()) <$> satisfy isUpper
