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

-- #1
ex1 :: a -> b -> b
ex1 = undefined

-- #2
ex2 :: a -> a -> a
ex2 = undefined

-- #3
ex3 :: Int -> a -> a
ex3 = undefined

-- #4
ex4 :: Bool -> a -> a -> a
ex4 = undefined

-- #5
ex5 :: Bool -> Bool
ex5 = undefined

-- #6
ex6 :: (a -> a) -> a
ex6 = undefined

-- #7
ex7 :: (a -> a) -> a -> a
ex7 = undefined

-- #8
ex8 :: [a] -> [a]
ex8 = undefined

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 = undefined

-- #10
ex10 :: Maybe a -> a
ex10 = undefined

-- #11
ex11 :: a -> Maybe a
ex11 = undefined

-- #12
ex12 :: Maybe a -> Maybe a
ex12 = undefined

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ element Leaf = Node Leaf element Leaf

-- #14
allCaps :: [String] -> Bool
allCaps [] = True
allCaps ("" : xs) = False
allCaps (x : xs) = let firstCharacter = head x
                   in firstCharacter > 'A' &&
                      firstCharacter < 'Z' &&
                      allCaps xs

-- #15
killWhiteSpaces :: String -> String
killWhiteSpaces (' ' : x) = killWhiteSpaces x
killWhiteSpaces x = x

dropTrailingWhitespace :: String -> String
dropTrailingWhitespace input = reverse $ killWhiteSpaces $ reverse input

-- #16
firstLetters :: [String] -> [Char]
firstLetters [] = []
firstLetters list = map head (filter (/= "") list)

-- #17
getListWithoutBrackets :: [String] -> String
getListWithoutBrackets [x] = x
getListWithoutBrackets (x:xs) = x ++ ", " ++ getListWithoutBrackets xs

asList :: [String] -> String
asList [] = "[]"
asList list = "[" ++ getListWithoutBrackets list ++ "]"
