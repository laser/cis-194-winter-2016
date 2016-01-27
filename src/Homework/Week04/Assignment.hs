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

import Data.Char  (isSpace, isUpper)
import Data.List  (intercalate)
import Data.Maybe (mapMaybe)

import Homework.Week04.BST

-- #1
-- only one implementation (basically just id, but throws away the first arg)
ex1 :: a -> b -> b
ex1 _ = id

-- #2
-- two possible implementations, could use the first or second arg
ex2 :: a -> a -> a
ex2 x _ = x

-- #3
-- still only one implementation, since we still only have exactly one “a”
ex3 :: Int -> a -> a
ex3 _ = id

-- #4
-- precisely four possible implementations:
--   - always return arg 2
--   - always return arg 3
--   - return arg 2 if arg 1 is True, otherwise arg 3
--   - return arg 3 if arg 1 is True, otherwise arg 2
ex4 :: Bool -> a -> a -> a
ex4 c a b = if c then a else b

-- #5
-- two possible implementations, either id or not
ex5 :: Bool -> Bool
ex5 = not

-- #6
-- impossible because we still need an “a” to get an “a”
ex6 :: (a -> a) -> a
ex6 = error "impossible"

-- #7
-- infinite possible implementations: could thread the second arg through the
-- first arg any number of times
ex7 :: (a -> a) -> a -> a
ex7 = ($)

-- #8
-- infinite possible implementations since the source can be used to create
-- arbitrary sublists, but the empty array can only return the empty array
ex8 :: [a] -> [a]
ex8 xs = take (length xs `div` 2) xs

-- #9
-- infinite possible implementations for the same reason as #8, but actually
-- getting any values requires effectively mapping
ex9 :: (a -> b) -> [a] -> [b]
ex9 = map

-- #10
-- impossible to implement due to the Nothing case
ex10 :: Maybe a -> a
ex10 = error "impossible"

-- #11
-- two possible implementations: Just or (const Nothing)
ex11 :: a -> Maybe a
ex11 = Just

-- #12
-- two possible implementations: id or (const Nothing)
ex12 :: Maybe a -> Maybe a
ex12 = id

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ x Leaf = Node Leaf x Leaf
insertBST cmp x (Node left y right)
  | x `cmp` y == LT = Node (insertBST cmp x left) y right
  | otherwise       = Node left y (insertBST cmp x right)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- #14
allCaps :: [String] -> Bool
allCaps = all isCapitalized
  where isCapitalized = maybe False isUpper . safeHead

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = reverse . dropWhile isSpace . reverse

-- #16
firstLetters :: [String] -> [Char]
firstLetters = mapMaybe safeHead

-- #17
asList :: [String] -> String
asList str = "[" ++ intercalate "," str ++ "]"
