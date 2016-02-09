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

import           Data.Char           (isUpper)
import           Data.Maybe          (catMaybes)
import           Homework.Week04.BST

-- #1
ex1 :: a -> b -> b
ex1 x y = y

-- #2
ex2 :: a -> a -> a
ex2 x _ = x

-- #3
ex3 :: Int -> a -> a
ex3 _ x = x

-- #4
ex4 :: Bool -> a -> a -> a
ex4 b x1 x2 = x1

-- #5
ex5 :: Bool -> Bool
ex5 = not

-- #6
ex6 :: (a -> a) -> a
ex6 = error "wat"

-- #7
ex7 :: (a -> a) -> a -> a
ex7 f x = f x

-- #8
ex8 :: [a] -> [a]
ex8 = id

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 f (x:xs) = f x : ex9 f xs

-- #10
ex10 :: Maybe a -> a
ex10 = error "wat"

-- #11
ex11 :: a -> Maybe a
ex11 = Just

-- #12
ex12 :: Maybe a -> Maybe a
ex12 = id

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ x Leaf = Node Leaf x Leaf
insertBST f x (Node t1 y t2) = case f x y of
  GT -> Node t1 y (insertBST f x t2)
  _ -> Node (insertBST f x t1) y t2

-- #14
allCaps :: [String] -> Bool
allCaps = foldl go True
  where
    go acc (c:cs) = acc && isUpper c
    go acc _ = False

-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = snd . foldr go (True, "")
  where
    go ' ' (True, cs)  = (True, cs)
    go ' ' (False, cs) = (False, ' ':cs)
    go c   (_, cs)     = (False, c:cs)

-- #16
firstLetters :: [String] -> [Char]
firstLetters = catMaybes . map go
  where
    go (c:cs) = Just c
    go _ = Nothing

-- #17
asList :: [String] -> String
asList [] = show ([] :: [String])
asList ss = foldr go "" (show ss)
  where
    go '"' cs = cs
    go c cs = c:cs
