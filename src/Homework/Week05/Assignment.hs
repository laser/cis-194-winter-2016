module Homework.Week05.Assignment (
  eval,
  evalStr,
  ExprT(..),
  -- uncomment these once you've defined them:
  -- Expr(..),
  -- MinMax(..),
  -- Mod7(..)
) where

--data ExprT = Lit Integer
--           | Add ExprT ExprT
--           | Mul ExprT ExprT
--  deriving (Show, Eq)

import Homework.Week05.ExprT
import Homework.Week05.Parser

-- #1
eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add expr1 expr2) = eval expr1 + eval expr2
eval (Mul expr1 expr2) = eval expr1 * eval expr2

-- #2
evalStr :: String -> Maybe Integer
evalStr s = maybe Nothing (Just . eval) (parseExp Lit Add Mul s)

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
