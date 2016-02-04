module Homework.Week05.Assignment (
  eval,
  evalStr,
  ExprT(..),
  -- uncomment these once you've defined them:
  Expr(..),
  -- MinMax(..),
  -- Mod7(..)
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
  lit x   = Lit x
  add x y = Add x y
  mul x y = Mul x y

-- #4
-- instance Expr Integer where
--   lit x = x
--   add x y = Add x y
--   mul x y = Add x y 
