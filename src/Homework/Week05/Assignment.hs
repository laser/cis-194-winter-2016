module Homework.Week05.Assignment (
  eval,
  evalStr,
  ExprT(..),
  -- uncomment these once you've defined them:
  Expr(..),
  MinMax(..),
  Mod7(..)
) where

import           Homework.Week05.ExprT
import           Homework.Week05.Parser

-- #1
eval :: ExprT -> Integer
eval (Lit v)             = v
eval (Mul x y)           = (eval x) * (eval y)
eval (Add x y)           = (eval x) + (eval y)

-- #2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

-- #3
class Expr e where
  lit :: Integer -> e
  add :: e -> e -> e
  mul :: e -> e -> e

instance Expr ExprT where
  lit     = Lit
  add x y = Add x y
  mul x y = Mul x y

-- #4
instance Expr Integer where
  lit x   = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x   = x > 0
  add     = (||)
  mul     = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
instance Expr MinMax where
  lit x   = MinMax x
  add     = max
  mul     = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show, Ord)

instance Expr Mod7 where
  lit x   = Mod7 $ x `mod` 7
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7
