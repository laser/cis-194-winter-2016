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

-- #1
-- Don't know how a and b interact
ex1 :: a -> b -> b
ex1 _ b = b

-- #2
-- Don't know how to operate on a.
-- Only implentation is to return one of the arguments.
-- In this case the first one.
ex2 :: a -> a -> a
ex2 a1 a2 = a1

-- #3
-- Don't know how Int and a interact.
-- Can only produce a from a.
ex3 :: Int -> a -> a
ex3 _ a = a

-- #4
ex4 :: Bool -> a -> a -> a
ex4 True a1 a2 = a1
ex4 False a1 a2 = a2

-- #5
-- This implementation negates the argument.
-- Could also return always True, always False, or the argument.
ex5 :: Bool -> Bool
ex5 True = False
ex5 False = True

-- #6
-- No input of type a provided, can't produce an a.
ex6 :: (a -> a) -> a
ex6 = error "impossible"

-- #7
-- Only possible implementations are apply the provided function a number of
-- times to input a to produce another a, including applying 0 times.
-- This implementation applies it twice.
ex7 :: (a -> a) -> a -> a
ex7 f a1 = f (f a1)

-- #8
-- We know how to operate on lists, so any operation on a list is valid here.
-- For example, every other element in the list, safeHead, identity, etc.
ex8 :: [a] -> [a]
ex8 as = as

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 f as = map f as

-- #10
-- Don't know how to produce an a in case the argument is Nothing.
ex10 :: Maybe a -> a
ex10 = error "imposible"

-- #11
-- The input is always an a and we don't know how to operate a, so the possible
-- implementations are always Just a or always Nothing.
ex11 :: a -> Maybe a
ex11 a = Just a

-- #12
-- Identity or always Nothing
ex12 :: Maybe a -> Maybe a
ex12 a = a

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ a Leaf = Node Leaf a Leaf
insertBST f x (Node left y right) = case f x y of
  LT -> Node (insertBST f x left) y right
  GT -> Node left y (insertBST f x right)
  EQ -> Node left y right

-- #14
allCaps :: [String] -> Bool
allCaps strings = all isWordUpper strings where
  isWordUpper "" = False
  isWordUpper (s:_) = isUpper s

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = reverse . dropLeadingWhitespace . reverse
  where dropLeadingWhitespace "" = ""
        dropLeadingWhitespace (x:xs)
          | isSpace x = dropLeadingWhitespace xs
          | otherwise = x:xs

-- #16
firstLetters :: [String] -> [Char]
firstLetters = map firstLetter . filterEmpty
  where filterEmpty = filter $ not . null
        firstLetter (s:_) = s

-- #17
asList :: [String] -> String
asList xs = "[" ++ (concat $ intersperse "," xs) ++ "]"
