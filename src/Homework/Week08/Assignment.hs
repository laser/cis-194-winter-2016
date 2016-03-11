module Homework.Week08.Assignment (
  first,
  abParser,
  abParser_,
  intPair,
  intOrUppercase
) where

import Homework.Week08.AParser
import Control.Applicative
import Control.Monad (join)
import Data.Functor (void)
import Data.Char (isUpper)


first :: (a -> b) -> (a , c) -> (b , c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap f p = Parser f'
     where f' = fmap (first f) . runParser p

-- #2
instance Applicative Parser where
  pure a  =  Parser (\s -> Just (a, s))
  f <*> v =  Parser (\s -> do
               (x, s0) <-  runParser f s
               (y, s1) <-  runParser v s0
               return (x y, s1))

-- #3
-- fmap (,) ? parseChar leftChar <*> parseChar rightChar
abParser :: Parser (Char, Char)
abParser =  (,) <$> (char 'a') <*> (char 'b')-- (char 'a') <*> (char 'b')


abParser_ :: Parser ()
abParser_ = void abParser

-- void' = fmap (const ())

intPair :: Parser [Integer]
intPair  =  (\i _ j -> [i,j]) <$> posInt <*> char ' ' <*> posInt

-- #4
instance Alternative Parser where
  empty   = Parser  (\_ -> Nothing)
  p <|> q = Parser  (\s -> runParser p s <|> runParser q s)

-- #5
intOrUppercase :: Parser ()
intOrUppercase = (void $ satisfy isUpper) <|> void posInt
