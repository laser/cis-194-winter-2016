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
import Control.Monad

-- #1
first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = ((f a), c)

instance Functor Parser where
  fmap fn (Parser g) = Parser f where
    f = fmap (first fn) . g

-- #2
instance Applicative Parser where
  pure empty = Parser f
    where f str = Just (empty, str)

  p1@(Parser f1) <*> p2@(Parser f2) = Parser f
    where
      f str = case f1 str of
        Nothing -> Nothing
        Just (aTob, str2) ->
          case f2 str2 of
            Nothing -> Nothing
            thing -> fmap (first aTob) thing

-- #3
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = pure f <*> abParser
  where
    f _ = ()

intPair :: Parser [Integer]
intPair = f <$> posInt <* char ' '<*> posInt
  where
    f x y = x:y:[]

-- #4
instance Alternative Parser where
  empty = Parser f where
    f _ = Nothing
  p1@(Parser f1) <|> p2@(Parser f2) = Parser f
    where
      f str = (f1 str) <|> (f2 str)


-- #5

intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy isUpper)
