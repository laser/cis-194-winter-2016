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
import Data.Char (isUpper, isSpace)
import Data.List (dropWhileEnd)
import Data.Maybe (listToMaybe, mapMaybe)

-- #1
ex1 :: a -> b -> b
ex1 _ x = x

-- #2
ex2 :: a -> a -> a
-- ex2 x _ = x
-- or
ex2 _ x = x

-- #3
ex3 :: Int -> a -> a
ex3 _ x = x

-- #4
ex4 :: Bool -> a -> a -> a
-- There are 4 distinct implementations:
-- ex4 _ x _ = x
-- OR
-- ex4 _ _ x = x
-- OR
-- ex4 True _ x = x
-- ex4 False x _ = x
-- OR
ex4 True x _ = x
ex4 False _ x = x

-- #5
ex5 :: Bool -> Bool
-- There are 4 distinct implementations:
-- ex5 _ = True
-- OR
-- ex5 - = False
-- OR
-- ex5 True = True
-- ex5 False = False
-- OR
ex5 True = False
ex5 False = True

-- #6
ex6 :: (a -> a) -> a
-- No input.
ex6 = error "impossible"

-- #7
ex7 :: (a -> a) -> a -> a
-- ex7 _ x = x
-- OR
ex7 f = f

-- #8
ex8 :: [a] -> [a]
-- List transformations (for example, `map`, `foldl/r`, `reverse`, `take`, `drop`, and so on) can be applied to the input.
-- OR
ex8 xs = xs

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 f = map f

-- #10
ex10 :: Maybe a -> a
-- I cannot give you an `a` if you give me `Nothing`.
ex10 = error "impossible"

-- #11
ex11 :: a -> Maybe a
-- ex11 _ = Nothing
-- OR
ex11 x = Just x

-- #12
ex12 :: Maybe a -> Maybe a
-- ex12 _ = Nothing
-- OR
ex12 (Just x) = Just x
ex12 _ = Nothing

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ x Leaf = Node Leaf x Leaf
insertBST cmp x (Node leftTree y rightTree) = case cmp x y of
    LT -> Node (insertBST cmp x leftTree) y rightTree
    _  -> Node leftTree y (insertBST cmp x rightTree)

-- #14
allCaps :: [String] -> Bool
allCaps = all $ maybe False isUpper . listToMaybe

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = dropWhileEnd isSpace

-- #16
firstLetters :: [String] -> [Char]
firstLetters = mapMaybe listToMaybe

-- #17
asList :: [String] -> String
asList = undefined
