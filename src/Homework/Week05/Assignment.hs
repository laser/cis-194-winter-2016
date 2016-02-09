module Homework.Week05.Assignment (
  eval,
  evalStr,
  ExprT(..),
  Expr(..),
  MinMax(..),
  Mod7(..)
) where

import           Homework.Week05.ExprT
import           Homework.Week05.Parser

import qualified Data.Map                as M
import qualified Homework.Week05.StackVM as StackVM

-- pass an expression and get back an integer
-- e.g. Lit 4 == 4
--      Add (Lit 1) (Lit 4) == 5
eval :: ExprT -> Integer
eval e1 = case e1 of
  (Lit n) -> n
  (Add e2 e3) -> eval e2 + eval e3
  (Mul e2 e3) -> eval e2 * eval e3

-- pass a string that can be parsed into an expression and
-- maybe get an integer from its evaluation
-- e.g. evalStr "1+5*2" == 12
evalStr :: String -> Maybe Integer
evalStr s = case (parseExp Lit Add Mul s) of
  Nothing -> Nothing
  (Just e) -> Just (eval e)

-- Create a type class called Expr with three methods: lit,
-- mul, and add which parallel the constructors of ExprT
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

-- Make an instance of Expr for the ExprT type in such a
-- way that:
-- (add (lit 4) (lit 6) :: ExprT) == Add (Lit 4) (Lit 6)
instance Expr ExprT where
  lit n = Lit n
  add e1 e2 = Add e1 e2
  mul e1 e2 = Mul e1 e2

-- Make instances of Expr for each of the following types:
--
-- Integer - works like the original
instance Expr Integer where
  lit n = n
  add n1 n2 = n1 + n2
  mul n1 n2 = n1 * n2

-- Bool - every literal value less than or equal to 0 is
--        interpreted as False and all positive Integers
--        are interpreted as True. Addition is logical or
--        and multiplication is logical and

instance Expr Bool where
  lit x = x > 0
  add y z = y || z
  mul y z = y && z

-- MinMax - addition is taken to be the max function and
--          multiplication is the min function
newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  add (MinMax y) (MinMax z) = MinMax (max y z)
  mul (MinMax y) (MinMax z) = MinMax (min y z)

-- Mod7 - all values should be in the range 0..6 and all
--        arithmetic is done modulo 7
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 y) (Mod7 z) = Mod7 ((y + z) `mod` 7)
  mul (Mod7 y) (Mod7 z) = Mod7 ((y * z) `mod` 7)
