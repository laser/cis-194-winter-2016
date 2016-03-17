module Homework.Week08.Assignment (
  first,
  abParser,
  abParser_,
  intPair,
  intOrUppercase
) where

import Homework.Week08.AParser
import Control.Applicative
import Data.Char (isUpper)

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Ex. 1 - implement a Functor instance for Parser

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
  fmap f1 p = Parser f2
    where
      f2 xs = case runParser p xs of
        Nothing  -> Nothing
        (Just v) -> Just $ first f1 v

-- Ex. 2 - implement an Applicative instance for Parser
--
-- A. pure a represents the parser which consumes no input
--    and successfully returns a result of a.
--
-- B. p1 <*> p2 represents the parser which ﬁrst runs p1
--    (which will consume some input and produce a
--    function), then passes the remaining input to p2
--    (which consumes more input and produces some value),
--    then returns the result of applying the function to
--    the value. However, if either p1 or p2 fails then the
--    whole thing should also fail (put another way, p1 <*>
--    p2 only succeeds if both p1 and p2 succeed).

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  (Parser p1) <*> p2 = Parser $ \s -> case p1 s of
    Nothing        -> Nothing
    (Just (f, xs)) -> runParser (f <$> p2) xs

-- Ex. 3a - Create a parser which expects to see the
-- characters ’a’ and ’b’ and returns them as a pair

abParser :: Parser (Char, Char)
abParser = (\x y -> (x, y)) <$> char 'a' <*> char 'b'

-- Ex. 3b - Create a parser which acts in the same way as
-- abParser but returns () instead of 'a' and 'b'
abParser_ :: Parser ()
abParser_ = (\x y -> ()) <$> char 'a' <*> char 'b'

-- Ex. 3c - Create a parser which reads two integer values
-- separated by a space and returns the integer values in a
-- list. You should use the provided posInt to parse the
-- integer values.

intPair :: Parser ([Integer])
intPair = (\x y z -> x:z:[]) <$> posInt <*> char ' ' <*> posInt

-- Ex. 4 - Write an Alternative instance for Parser
--
-- See: http://hackage.haskell.org/package/base-4.7.0.0/docs/Control-Applicative.html#t:Alternative
--
-- empty represents the parser which always fails.
-- p1 <|> p2 represents the parser which ﬁrst tries running
-- p1. If p1 succeeds then p2 is ignored and the result of
-- p1 is returned.  Otherwise, if p1 fails, then p2 is
-- tried instead.
--
-- Hint: there is already an Alternative instance for Maybe
-- which you may find useful.

instance Alternative Parser where
  empty = Parser (\s -> Nothing)
  p1 <|> p2 = Parser $ \s -> case runParser p1 s of
    Nothing -> runParser p2 s
    x       -> x

-- Ex. 5 - Implement a parser:
--
-- intOrUppercase :: Parser ()
--
-- which parses either an integer value or an uppercase
-- character, and fails otherwise.

intOrUppercase :: Parser ()
intOrUppercase = const () <$> satisfy isUpper <|> const () <$> posInt