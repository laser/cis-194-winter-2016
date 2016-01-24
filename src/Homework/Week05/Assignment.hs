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
    lit n = Lit n
    add e1 e2 = Add e1 e2
    mul e1 e2 = Mul e1 e2

-- #4
instance Expr Integer where
   lit e = e
   add e1 e2 = e1 + e2
   mul e1 e2 = e1 * e2

instance Expr Bool where
   lit n = if n <=0 then False else True
   add e1 e2 = e1 || e2
   mul e1 e2 = e1 && e2

newtype MinMax = MinMax Integer deriving (Eq,Ord,Show)

instance Expr MinMax where
   lit n = MinMax n
   add e1 e2 = max e1 e2
   mul e1 e2 = min e1 e2

newtype Mod7 = Mod7 Integer deriving (Eq,Show)

instance Expr Mod7 where
   lit n = Mod7 (mod n 7)
   add (Mod7 n1) (Mod7 n2) = Mod7 (mod (n1 + n2) 7)
   mul (Mod7 n1) (Mod7 n2) = Mod7 (mod (n1 * n2) 7)


