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
ex2 a _ = a

-- #3
ex3 :: Int -> a -> a
ex3 _ a = a

-- #4
ex4 :: Bool -> a -> a -> a
ex4 _ a _ = a

-- #5
ex5 :: Bool -> Bool
ex5 x = x

-- #6
ex6 :: (a -> a) -> a
ex6 = error "impossible"

-- #7
ex7 :: (a -> a) -> a -> a
ex7 x = x

-- #8
ex8 :: [a] -> [a]
ex8 x = x

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 f = map f

-- #10
ex10 :: Maybe a -> a
ex10 = error "impossible"

-- #11
ex11 :: a -> Maybe a
ex11 = Just

-- #12
ex12 :: Maybe a -> Maybe a
ex12 x = x

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ x Leaf = Node Leaf x Leaf
insertBST f x tree@(Node left y right) =
    case (f x y) of EQ -> tree
                    GT -> insertBST f x right
                    LT -> insertBST f x left

-- #14
safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead [] = Nothing

allCaps :: [String] -> Bool
allCaps xs = notElem False $ catMaybes $ map (safeHead . map isUpper) xs

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace x = reverse $ dropWhile isSpace $ reverse x

-- #16
firstLetters :: [String] -> [Char]
firstLetters = catMaybes . map safeHead

-- #17
asList :: [String] -> String
asList xs = "[" ++ (intercalate  ", " xs) ++ "]"
