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
ex1 _ x = x

-- #2
ex2 :: a -> a -> a
ex2 _ x = x

-- #3
ex3 :: Int -> a -> a
ex3 _ x = x

-- #4
ex4 :: Bool -> a -> a -> a
ex4 _ _ x = x
ex4 _ x _ = x

-- #5
ex5 :: Bool -> Bool
ex5 x = x
ex5 x = True
ex5 x = False

-- #6
ex6 :: (a -> a) -> a
ex6 = error "impossible"

-- #7
ex7 :: (a -> a) -> a -> a
ex7 f x = f $ x

-- #8
ex8 :: [a] -> [a]
ex8 x = x

-- #9
ex9 :: (a -> b) -> [a] -> [b]
ex9 f x = map f x

-- #10
ex10 :: Maybe a -> a
ex10 = undefined

-- #11
ex11 :: a -> Maybe a
ex11 x = Just x

-- #12
ex12 :: Maybe a -> Maybe a
ex12 x = x

-- #13
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ x Leaf = Node Leaf x Leaf
insertBST comparator x (Node l e r) = case comparator x e of
                                        GT -> Node l e ( insertBST comparator x r )
                                        _  -> Node (insertBST comparator x l) e r

-- #14
allCaps :: [String] -> Bool
allCaps [] = True
allCaps strings = case find (==False) (map isCapitalized strings) of
                    Nothing -> True
                    Just _ -> False
    where
        isCapitalized [] = False
        isCapitalized (x:_) = isUpper x



-- #15
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = dropWhileEnd isSpace

-- #16
firstLetters :: [String] -> [Char]
firstLetters = catMaybes . map listToMaybe

-- #17
asList :: [String] -> String
asList xs = "[" ++ intercalate ", " xs ++ "]"
