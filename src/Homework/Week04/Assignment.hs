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
import Data.List (intercalate, dropWhileEnd)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Function (fix)

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
ex6 :: (a -> a) -> a
ex6 = fix -- thanks jvargas, I never would have found this otherwise

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
ex10 Nothing = error "impossible because we can't return anything of type 'a'."

-- #11
ex11 :: a -> Maybe a
ex11 a = Just a

-- #12
ex12 :: Maybe a -> Maybe a
ex12 = id

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST f a Leaf = Node Leaf a Leaf
insertBST f a (Node left b right) = 
    if f a b == LT 
        then Node (insertBST f a left) b right
        else Node left b (insertBST f a right)

-- #14
allCaps :: [String] -> Bool
allCaps = all isUpperCase  
    where isUpperCase = maybe False isUpper . listToMaybe

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = dropWhileEnd isSpace

-- #16
firstLetters :: [String] -> [Char]
firstLetters = mapMaybe listToMaybe

-- #17
asList :: [String] -> String
asList ss = "[" ++ (intercalate ", " ss) ++ "]"
