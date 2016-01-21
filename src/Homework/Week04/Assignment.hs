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
  ,safeHead
  ,safeTail
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
ex2 a _ = a

-- #3
ex3 :: Int -> a -> a
ex3 _ a = a

-- #4
ex4 :: Bool -> a -> a -> a
ex4 _ a _ = a

-- #5
ex5 :: Bool -> Bool
ex5 = id 

-- #6
ex6 :: (a -> a) -> a
ex6 f = error "impossible"

-- #7
ex7 :: (a -> a) -> a -> a
ex7  = id 

-- #8
ex8 :: [a] -> [a]
ex8 = id

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 f = map f 

-- #10
ex10 :: Maybe a -> a
ex10  = error "impossible"

-- #11
ex11 :: a -> Maybe a
ex11 = Just 

-- #12
ex12 :: Maybe a -> Maybe a
ex12  = id

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ el Leaf = Node Leaf el Leaf
insertBST cmp el tree@(Node left visit right) = 
  case (cmp el visit) of
     EQ -> tree
     LT -> insertBST cmp el left
     GT -> insertBST cmp el right
    
safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x: _) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_ : xs) = Just xs

-- #14
allCaps :: [String] -> Bool
allCaps xs = length xs == (length $ takeWhile (==True) $  catMaybes $  map (safeHead . map isUpper) xs)

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = reverse . dropWhile (== ' ' ) . reverse

-- #16
firstLetters :: [String] -> [Char]
firstLetters = catMaybes . map safeHead

-- #17
asList :: [String] -> String
asList xs = "[" ++ ( concat $ intersperse ", " xs ) ++ "]"
