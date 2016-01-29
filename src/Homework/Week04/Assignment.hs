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

import           Data.Char
import           Data.List
import           Data.String
import           Homework.Week04.BST

-- #1
ex1 :: a -> b -> b
ex1 = seq
--because the types are totally vague, the only thing our fn(s) could decide is
--which parameter to return. Because the result must be of the second parameter's
--type, there is only one distinct function possible.

-- #2
ex2 :: a -> a -> a
ex2 = const
--this is a more permissive (in terms of the amount of distinct functions inhabiting
-- the type) version of ex1. distinct functions can return either the first or second
-- parameter, so there are two possible distinct functions.

-- #3
ex3 :: Int -> a -> a
ex3 = seq
--b/c the 2nd param could be anything, there's no way to use the Int to make a decision
--about what to return. Therefore, there is only one possible distinct fn.
-- #4
ex4 :: Bool -> a -> a -> a
ex4 b a a' = if b then a else a'
--4 distinct functions: b -> a, b -> a', not b -> a, not b -> a'

-- #5
ex5 :: Bool -> Bool
ex5 = not
--2 distinct fns: not and id

-- #6
ex6 :: (a -> a) -> a
ex6 = error "impossible"
--without a non-fn value to return, the fn can't return a value

-- #7
ex7 :: (a -> a) -> a -> a
ex7 fn = fn
--simply applying the given fn to the 2nd param is the only thing that works for all types.

-- #8
ex8 :: [a] -> [a]
ex8 = reverse
--infinite possible distinct fns? id, reverse, [first], [second], (etc), any variation on scrambling the list...

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 = map
--infinite distinct fns? map, map reverse, replicate 2 map, replicate 3 map...

-- #10
ex10 :: Maybe a -> a
ex10 = error "impossible"
--any possible fn fails if Maybe is Nothing

-- #11
ex11 :: a -> Maybe a
ex11 = Just
--only one. anything that doesn't boil down to Just has the possibility of returning Nothing, which can't be of type a.

-- #12
ex12 :: Maybe a -> Maybe a
ex12 = id
--only one. same reason.

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

-- #13
insertBST :: Ord a => (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ x Leaf = Node Leaf x Leaf
insertBST cmp x tree@(Node lt bdy rt)
  | order == LT || order == EQ   = Node (insertBST compare x lt) bdy rt
  | order == GT                  = Node lt bdy (insertBST compare x rt)
  where order = cmp x bdy

-- #14
allCaps :: [String] -> Bool
allCaps = all $ maybe False isUpper . safeHead

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = reverse . dropWhile isSpace . reverse

-- #16
firstLetters :: [String] -> String
firstLetters = concatMap (take 1)

-- #17
asList :: [String] -> String
asList x = "[" ++ intercalate ", " x ++ "]"
