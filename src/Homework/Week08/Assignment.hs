module Homework.Week08.Assignment (
  first,
  abParser,
  abParser_,
  intPair,
  intOrUppercase
) where

import Homework.Week08.AParser
import Control.Applicative
import Data.Char

-- #1
first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)

instance Functor Parser where
  fmap fab (Parser fa) = Parser (\x -> fmap (first fab) (fa x))

-- #2
instance Applicative Parser where
  pure = \a -> Parser (\b -> Just(a, b))
  (Parser fab) <*> (Parser fa) = Parser f
        where f a = case (fab a) of
                        Nothing -> Nothing
                        Just(a', rem) -> fmap (first a') (fa rem)


  --Parser (\x -> fmap (fb) (fa x))

-- #3

abParser :: Parser (Char, Char)
abParser = (fmap (\a b -> (a,b)) (char 'a')) <*> (char 'b')

abParser_ :: Parser ()
abParser_ = (\a b -> ()) <$> (char 'a') <*> (char 'b')

intPair :: Parser [Integer]
intPair = (\a _ b -> a:b:[]) <$> posInt <*> char ' ' <*> posInt

-- #4
instance Alternative Parser where
  empty = Parser (\x -> Nothing)
  (Parser fa) <|> (Parser fb)  = Parser f
        where f a = case (fa a) of
                        Nothing -> fb a
                        Just(a', rem) -> Just(a', rem)

-- #5
-- upperCase :: Char -> Parser Char
-- upperCase = satisfy $ isUpper

intOrUppercase :: Parser ()
intOrUppercase = ((\_ -> ()) <$> posInt) <|> ((\_ -> ()) <$> satisfy isUpper)
