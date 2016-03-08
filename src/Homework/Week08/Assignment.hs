module Homework.Week08.Assignment (
  first,
  abParser,
  abParser_,
  intPair,
  intOrUppercase
) where

import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import           Homework.Week08.AParser

-- #1
first :: (a -> b) -> (a,c) -> (b,c)
first f (x, y) = (f x, y)

instance Functor Parser where
  fmap f p = Parser $ fmap (first f) . runParser p

-- #2
instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  -- p1 <*> p2 = fmap f p2
  --   where f = maybe _ fst . runParser p1
  -- nothing can satisfy the type of the hole...
  p1 <*> p2 = Parser $ \s -> do
    (f, rest1) <- runParser p1 s
    (x, rest2) <- runParser p2 rest1
    return (f x, rest2)

-- #3
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\x y -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\x y -> [x,y]) <$> posInt <* char ' ' <*> posInt

-- #4
instance Alternative Parser where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \s ->
    if isJust $ runParser p1 s
      then runParser p1 s
      else runParser p2 s

-- #5
intOrUppercase :: Parser ()
intOrUppercase = void $ void posInt <|> void upCaseChar

upCaseChar :: Parser Char
upCaseChar = satisfy (`elem` ['A'..'Z'])
