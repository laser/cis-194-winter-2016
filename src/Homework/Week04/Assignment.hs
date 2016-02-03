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
import Data.Function (fix)
import Data.Maybe (fromJust) -- bad
import Data.Char (isUpper)
import Data.Maybe (catMaybes)
import Data.List (intercalate)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- #1
ex1 :: a -> b -> b
ex1 _ = id

-- #2
ex2 :: a -> a -> a
ex2 a _ = a

-- #3
ex3 :: Int -> a -> a
ex3 _ a = a

-- #4
ex4 :: Bool -> a -> a -> a
ex4 cond a b = if cond then a else b

-- #5
ex5 :: Bool -> Bool
ex5 = not

-- #6
ex6 :: (a -> a) -> a
ex6 y = y (fix y)

-- #7
ex7 :: (a -> a) -> a -> a
ex7 f = f

-- #8
ex8 :: [a] -> [a]
ex8 _ = []

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 f = map f

-- #10
ex10 :: Maybe a -> a
ex10 = fromJust -- bad

-- #11
ex11 :: a -> Maybe a
ex11 = Just

-- #12
ex12 :: Maybe a -> Maybe a
ex12 _ = Nothing

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ x Leaf = Node Leaf x Leaf
insertBST cmp x (Node a y b) = case cmp x y of
  LT -> Node (insertBST cmp x a) y b
  EQ -> Node (insertBST cmp x a) y b
  GT -> Node a y (insertBST cmp x b)

-- #14
allCaps :: [String] -> Bool
allCaps = all (maybe False isUpper . safeHead)

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = reverse . dropWhile (== ' ') . reverse

-- #16
firstLetters :: [String] -> [Char]
firstLetters = catMaybes . map safeHead

-- #17
asList :: [String] -> String
asList xs = "[" ++ intercalate ", "xs ++ "]"
