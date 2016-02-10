module Homework.Week05.Assignment (
  eval,
  evalStr,
  ExprT(..),
  Expr(..),
  MinMax(..),
  Mod7(..)
) where

import Homework.Week05.ExprT
import Homework.Week05.Parser

-- #1
eval :: ExprT -> Integer
eval (Lit x)   = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- #2
evalStr :: String -> Maybe Integer
evalStr = maybe Nothing (Just . eval) . parseExp Lit Add Mul

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
  lit = ( > 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer
  deriving (Eq, Show, Ord)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

newtype Mod7 = Mod7 Integer
  deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7 . ( `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7
