module Homework.Week05.Assignment (
  eval,
  evalStr,
  ExprT(..),
  -- uncomment these once you've defined them:
  Expr(..),
  MinMax(..),
  Mod7(..)
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
   add = (+)
   mul = (*)

instance Expr Bool where
    lit = (> 0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Ord MinMax where
    (<=) (MinMax x) (MinMax y) = x <= y

instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
    mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)