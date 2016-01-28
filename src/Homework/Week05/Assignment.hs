{-# LANGUAGE FlexibleInstances #-}
module Homework.Week05.Assignment (
  eval,
  evalStr,
  ExprT(..),
  Expr(..),
  MinMax(..),
  Mod7(..)
 ,ExprT(..)
 ,Listable(..)
) where

import Homework.Week05.ExprT
import Homework.Week05.Parser
import Data.Char

f :: a -> b
f a = error "naaah gaaah daaah"


class Listable a where
  toList :: a -> [Int]

instance Listable Int where
  toList x = [x]

instance Listable Bool where
  toList True  = [1]
  toList False = [0]

instance Listable [Bool] where
  toList = concatMap toList

instance Listable [Int] where
  toList = id

instance Listable String where
  toList = map ord

instance (Listable a , Listable b) => Listable (a,b) where
  toList (a,b) = toList a ++ toList b

-- #1
eval :: ExprT -> Integer
eval (Lit a) =  a
eval (Add a b) = (eval a)  + (eval b)
eval (Mul a b) = (eval a)  * (eval b)

-- #2
evalStr :: String -> Maybe Integer
evalStr s = undefined -- parseExp Lit Add Mul s

-- #3
-- class Expr a where
--   lit :: ???
--   add :: ???
--   mul :: ???

-- #4
-- instance Expr Integer where
--   lit = ???
--   add = ???
--   mul = ???
