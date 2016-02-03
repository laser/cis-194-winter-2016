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

-- #1
eval :: ExprT -> Integer
eval (Lit v) = v
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

-- #2
evalStr :: String -> Maybe Integer
evalStr = parseExp id (+) (*)


-- #3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- #4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit a = if a <= 0 then False else True
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer
  deriving (Show, Eq)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

newtype Mod7 = Mod7 Integer
  deriving (Show, Eq)

instance Expr Mod7 where
  lit = Mod7 . mod7
  add (Mod7 a) (Mod7 b) = Mod7 (mod7 $ a + b)
  mul (Mod7 a) (Mod7 b) = Mod7 (mod7 $ a * b)

mod7 :: Integer -> Integer
mod7 = flip mod 7
