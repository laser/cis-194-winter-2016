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
import Data.Char
import Data.List
import Data.Maybe


-- #1
ex1 :: a -> b -> b
ex1 _ b = b

-- #2
ex2 :: a -> a -> a
ex2 _ a = a

-- #3
ex3 :: Int -> a -> a
ex3 _ a = a

-- #4
ex4 :: Bool -> a -> a -> a
ex4 _ _ a = a

-- #5
ex5 :: Bool -> Bool
ex5 a = a

-- #6
ex6 :: (a -> a) -> a
ex6 = error "impossible"

-- #7
ex7 :: (a -> a) -> a -> a
ex7 f a = a

-- #8
ex8 :: [a] -> [a]
ex8 x = x

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 f x = map f x

-- #10
ex10 :: Maybe a -> a
ex10 = error "impossible"

-- #11
ex11 :: a -> Maybe a
ex11 a = Just a

-- #12
ex12 :: Maybe a -> Maybe a
ex12 x = x

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST f a Leaf = Node Leaf a Leaf
insertBST f a (Node left b right)
  | f a b == LT = Node (insertBST f a left) b right
  | f a b == EQ = (Node left b right)
  | otherwise = Node left b (insertBST f a right)

doesStringStartWithUpperCase :: String -> Bool
doesStringStartWithUpperCase "" = False
doesStringStartWithUpperCase (x:xs) = isUpper x

-- #14
allCaps :: [String] -> Bool
allCaps [] = True
allCaps list = all doesStringStartWithUpperCase list

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace x = dropWhileEnd isSpace x

-- #16
firstLetters :: [String] -> [Char]
firstLetters [] = []
firstLetters (x:xs)
  | listToMaybe x == Nothing = firstLetters xs
  | otherwise = head x : firstLetters xs

-- #17
asList :: [String] -> String
asList (list) = "[" ++ (concat $ intersperse "," list) ++ "]"
