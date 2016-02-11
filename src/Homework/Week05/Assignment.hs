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
eval (Lit x) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- #2
evalStr :: String -> Maybe Integer
evalStr = undefined

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
