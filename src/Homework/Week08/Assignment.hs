{-# OPTIONS_GHC -fno-warn-orphans #-}

module Homework.Week08.Assignment (
  first,
  abParser,
  abParser_,
  intPair,
  intOrUppercase
) where

import Homework.Week08.AParser

import Control.Applicative

import qualified Control.Arrow       as Arr (first)
import           Control.Monad              (void)
import           Data.Char                  (isUpper)

-- #1
first :: (a -> b) -> (a, c) -> (b, c)
first = Arr.first

instance Functor Parser where
  fmap f (Parser g) = Parser $ fmap (first f) . g

-- #2
instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)

  (Parser a) <*> (Parser b) = Parser $ \str -> do
    (f, remainingA) <- a str
    (x, remainingB) <- b remainingA
    return (f x, remainingB)

-- #3
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = void abParser

intPair :: Parser [Integer]
intPair = collect <$> posInt <* char ' ' <*> posInt
  where collect a b = [a, b]

-- #4
instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser a) <|> (Parser b) = Parser $ \str -> a str <|> b str

-- #5
intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy isUpper)
