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
  safeHead,
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
import Data.Function

-- #1
ex1 :: a -> b -> b
ex1 = flip const

-- #2
ex2 :: a -> a -> a
ex2 = const

-- #3
ex3 :: Int -> a -> a
ex3 x y = if ( x> 0) then y else y

-- #4
ex4 :: Bool -> a -> a -> a
ex4 p x y = if p then x else y

-- #5
ex5 :: Bool -> Bool
ex5 = not

-- #6
ex6 :: (a -> a) -> a
ex6 = fix

-- #7
ex7 :: (a -> a) -> a -> a
ex7 =($)

-- #8
ex8 :: [a] -> [a]
ex8 = reverse

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 = map

-- #10
ex10 :: Maybe a -> a
ex10 = fromJust

-- #11
ex11 :: a -> Maybe a
ex11 = return

-- #12
ex12 :: Maybe a -> Maybe a
ex12 = id


-- cmp :: (Ord a) => a -> a -> Ordering
-- cmp x y
--   | x == y = EQ
--   | x > y  = GT
--   | x < y  = LT

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ x Leaf = Node Leaf x Leaf
insertBST fn x (Node lft v rt) = case (fn x v) of
     EQ -> Node lft x rt
     GT -> Node lft v (insertBST fn x rt)
     LT -> Node (insertBST fn x lft) v rt



safeHead :: [a] -> Maybe a
safeHead xs = case xs of
    [] -> Nothing
    (x : _) -> Just x

-- #14
allCaps :: [String] -> Bool
allCaps = all $ maybe False isUpper . safeHead
-- allCaps = all isUpper . firstLetters

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = reverse . dropWhile (==' ') . reverse

-- #16
firstLetters :: [String] -> [Char]
--firstLetters strLst = filter (not . isSpace) $ (map $ maybe ' ' id . safeHead) strLst
firstLetters = concatMap $ (take 1) . dropWhile isSpace


-- #17
asList :: [String] -> String
asList lst = "[" ++ intercalate aComma lst ++ "]"
   where aComma = ", "
