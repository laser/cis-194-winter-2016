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
import Data.List (intercalate)
import Data.Maybe (mapMaybe)

-- #1
ex1 :: a -> b -> b
ex1 _ b = b

-- #2
ex2 :: a -> a -> a
ex2 a _ = a

-- #3
ex3 :: Int -> a -> a
ex3 _ b = b

-- #4
ex4 :: Bool -> a -> a -> a
ex4 _ _ a = a

-- #5
ex5 :: Bool -> Bool
ex5 = id

-- #6
-- impossible 
ex6 :: (a -> a) -> a
ex6 = undefined

-- #7
ex7 :: (a -> a) -> a -> a
ex7 f a = a

-- #8
ex8 :: [a] -> [a]
ex8 = id

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 f a = map f a

-- #10
ex10 :: Maybe a -> a
ex10 (Just a) = a
ex10 Nothing = error "sorry"

-- #11
ex11 :: a -> Maybe a
ex11 a = Just a

-- #12
ex12 :: Maybe a -> Maybe a
ex12 = id

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST f a Leaf = (Node Leaf a Leaf)

-- #14
allCaps :: [String] -> Bool
allCaps = all isFirstUpper 
    where isFirstUpper []    = False
          isFirstUpper (s:_) = isUpper s

-- #15

dropLastWhile :: (Char -> Bool) -> String -> String
dropLastWhile _ [] = []
dropLastWhile f xs = if f (last xs) then dropLastWhile f (init xs) else xs

dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = dropLastWhile isSpace

-- #16
firstLetters :: [String] -> [Char]
firstLetters = mapMaybe firstLetter
    where firstLetter []    = Nothing
          firstLetter (x:_) = Just x

-- #17
asList :: [String] -> String
asList ss = "[" ++ (intercalate ", " ss) ++ "]"
