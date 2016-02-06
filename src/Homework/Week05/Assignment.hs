module Homework.Week05.Assignment (
  eval,
  evalStr,
  ExprT(..),
  -- uncomment these once you've defined them:
  Expr(..),
  MinMax(..),
   Mod7(..)
) where

import Homework.Week05.ExprT
import Homework.Week05.Parser
import Data.Maybe

-- #1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add left right) =  (eval left) + (eval right)
eval (Mul left right) = (eval left) * (eval right)

-- #2
evalStr :: String -> Maybe Integer
evalStr input =
  if parseExp Lit Add Mul input == Nothing
    then Nothing
  else Just(eval(fromJust(parseExp Lit Add Mul input)))

-- #3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

-- #4
instance Expr Integer where
  lit a = a
  add a b = a + b
  mul a b = a * b

instance Expr ExprT where
  lit a = Lit a
  add a b = Add a b
  mul a b = Mul a b

instance Expr Bool where
  lit a =
    if a > 0 then True
    else False
  add a b = a || b
  mul a b = a && b

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit a = MinMax a
  add (MinMax a) (MinMax b) =
    if a > b then MinMax a
    else MinMax b
  mul (MinMax a) (MinMax b) =
    if a > b then MinMax b
    else MinMax a

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit a = Mod7 (a `mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 ((a+b) `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 ((a*b) `mod` 7)
