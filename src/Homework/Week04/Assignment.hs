module Homework.Week04.Assignment (
  ex1,
  ex2,
  ex3,
  ex4,
  ex5,
  ex6,
  ex7,
  ex8,
  ex9,
  ex10,
  ex11,
  ex12,
  insertBST,
  allCaps,
  dropTrailingWhitespace,
  firstLetters,
  asList,
  BST(..)
) where

import Homework.Week04.BST
import Data.Char (isUpper)

-- #1
ex1 :: a -> b -> b
-- always return the second argument
ex1 x y = y

-- #2
ex2 :: a -> a -> a
-- always return the first argument
ex2 x y = x

-- #3
ex3 :: Int -> a -> a
-- same as ex1, but the irst arg is an int
ex3 = ex1

-- #4
ex4 :: Bool -> a -> a -> a
-- my impl is a simple if
-- five possible impls
--  always second arg
--  always third arg
--  if first arg then second else third
--  if first arg then third else second
--  returns randomly second or third
ex4 True y z = y
ex4 False y z = z

-- #5
ex5 :: Bool -> Bool
-- five possible impls
--  1) always true
--  2) always false
--  3) returns input
--  4) returns opposite of input (this is implemented below)
--  5) returns random boolean
ex5 x = not x

-- #6
ex6 :: (a -> a) -> a
ex6 = error "impossible"

-- #7
ex7 :: (a -> a) -> a -> a
-- two possible impls
-- 1) return second arg
-- 2) returns result of first arg execed with second arg
ex7 fx x = fx x

-- #8
ex8 :: [a] -> [a]
-- infinite number of args
-- my impl reverses the list
ex8 xs = reverse xs

-- #9
ex9 :: (a -> b) -> [a] -> [b]
-- only solution is to map from list of a's to list of b's
ex9 fx alist = map fx alist

-- #10
ex10 :: Maybe a -> a
-- no total solution possible
ex10 = error "impossible"

-- #11
ex11 :: a -> Maybe a
-- two possible solutions
-- 1) return Just a
-- 2) always return Nothing
ex11 _ = Nothing

-- #12
ex12 :: Maybe a -> Maybe a
-- two possible solutions
-- 1) return input
-- 2) always return Nothing
ex12 _ = Nothing

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ x Leaf                    = Node Leaf x Leaf
insertBST ordering newval (Node left oldval right)
       | ordering newval oldval == LT = Node (insertBST ordering newval left) oldval  right
       | ordering newval oldval == GT = Node left oldval (insertBST ordering newval right)
       | otherwise                    = Node left oldval right

-- #14
thisCap :: String -> Bool
thisCap (x:ys) = isUpper x
thisCap _ = False

allCaps :: [String] -> Bool
allCaps xs = all thisCap xs
-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = undefined

-- #16
firstLetters :: [String] -> [Char]
firstLetters = undefined

-- #17
asList :: [String] -> String
asList = undefined
