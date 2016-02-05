module Homework.Week05.Assignment (
  eval,
  evalStr,
  ExprT(..),
  -- uncomment these once you've defined them:
  Expr(..),
  MinMax(..),
  Mod7(..),
  mod7
) where

import Homework.Week05.ExprT
import Homework.Week05.Parser
import Control.Monad (liftM)

-- #1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)

-- #2
evalStr :: String -> Maybe Integer
--(fmap eval) :: Functor f => f ExprT -> f Integer . (String -> Maybe ExpT)
-- lift or fmap:
evalStr = fmap eval . parseExp Lit Add Mul

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
  lit x   = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit  = (> 0)
  add x y = x || y
  mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
    lit  = MinMax
    add (MinMax x) (MinMax y)  = MinMax $ max x y
    mul (MinMax x) (MinMax y)  = MinMax $ min x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

mod7 :: Integer -> Integer
mod7 x = x `mod` 7

instance Expr Mod7 where
    lit x  = Mod7 $ mod7 x
    add (Mod7 x) (Mod7 y)  = Mod7 $ mod7 (x + y)
    mul (Mod7 x) (Mod7 y)  = Mod7 $ mod7 (x * y)
