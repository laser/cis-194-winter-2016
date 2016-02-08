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
 ,Expr(..)
 ,reify
 ,testInteger,testBool,testMM,testSat
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
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

-- #2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

-- #3
class Expr a where
   lit :: Integer -> a
   add :: a -> a -> a
   mul :: a -> a -> a

-- #4
instance Expr Integer where
  lit =  id
  add =  (+)
  mul =  (*)

instance Expr ExprT  where
  lit  = Lit
  add  = Add
  mul  = Mul

reify :: ExprT -> ExprT
reify = id

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq,Show)
newtype Mod7   = Mod7 Integer deriving (Eq,Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 a) (Mod7 b) = Mod7 $ mod ( a + b ) 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ mod ( a * b ) 7


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

-- # Execise 4 test
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

