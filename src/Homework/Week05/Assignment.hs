module Homework.Week05.Assignment (
  eval,
  evalStr,
  ExprT(..)
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
-- class Expr a where 
--   lit :: ???
--   add :: ???
--   mul :: ???

-- #4
-- instance Integer Expr where
--   lit = ???
--   add = ???
--   mul = ???
