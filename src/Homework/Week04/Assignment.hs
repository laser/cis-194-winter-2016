module Homework.Week04.Assignment (
  ex1,
  ex2,
  ex3,
  ex4,
  ex5,
  -- ex6,
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
ex1 _ x = x

-- #2
ex2 :: a -> a -> a
ex2 x _ = x

-- #3
ex3 :: Int -> a -> a
ex3 _ x = x

-- #4
ex4 :: Bool -> a -> a -> a
ex4 _ _ x = x

-- #5
ex5 :: Bool -> Bool
ex5 x = x && True

-- #6
ex6 :: (a -> a) -> a
ex6 = error "impossible"

-- #7
ex7 :: (a -> a) -> a -> a
ex7 fun = fun

-- #8
ex8 :: [a] -> [a]
ex8 = take 3

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 = map

-- #10
ex10 :: Maybe a -> a
ex10 = error "impossible"

-- #11
ex11 :: a -> Maybe a
ex11 = Just

-- #12
ex12 :: Maybe a -> Maybe a
ex12 x = case x of
          Just y -> Just y
          Nothing -> Nothing

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _ ) = Just x

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ element Leaf = Node Leaf element Leaf

-- #14
allCaps :: [String] -> Bool
allCaps [] = True
allCaps (x : xs) = case safeHead x of
                        Just a -> a > 'A' && a < 'Z' && allCaps xs
                        Nothing -> False

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
getListWithoutBrackets (x:xs) = x ++ "," ++ getListWithoutBrackets xs

asList :: [String] -> String
asList [] = "[]"
asList list = "[" ++ getListWithoutBrackets list ++ "]"
