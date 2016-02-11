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

--data ExprT = Lit Integer
--           | Add ExprT ExprT
--           | Mul ExprT ExprT
--  deriving (Show, Eq)

-- #1
eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add expr1 expr2) = eval expr1 + eval expr2
eval (Mul expr1 expr2) = eval expr1 * eval expr2

-- #2
evalStr :: String -> Maybe Integer
evalStr s = maybe Nothing (Just . eval) (parseExp Lit Add Mul s)

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
   lit a = a
   add a b = a + b
   mul a b = a * b

instance Expr Bool where
    lit x = x > 0
    add a b = a || b
    mul a b = a && b
