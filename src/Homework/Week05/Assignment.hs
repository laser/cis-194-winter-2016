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
eval e = case e of
    Lit n -> n
    Add e1 e2 -> eval e1 + eval e2
    Mul e1 e2 -> eval e1 * eval e2

-- #2
evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
    Just expr -> Just (eval expr)
    Nothing   -> Nothing

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
   lit = (> 0)
   add = (||)
   mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq,Ord,Show)

instance Expr MinMax where
   lit = MinMax
   add = max
   mul = min

newtype Mod7 = Mod7 Integer deriving (Eq,Show)

mod7 :: Integer -> Mod7
mod7 n = Mod7 (mod n 7)

instance Expr Mod7 where
   lit = mod7
   add (Mod7 n1) (Mod7 n2) = mod7 (n1 + n2)
   mul (Mod7 n1) (Mod7 n2) = mod7 (n1 * n2)

