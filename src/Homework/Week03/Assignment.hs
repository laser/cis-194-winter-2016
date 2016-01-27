module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

import Data.List (transpose)

-- #1
skips :: [a] -> [[a]]
skips xs = let
  skip [] _ _ = []
  skip (y:ys) acc goal = if acc == goal then y : skip ys 0 goal else skip ys (succ acc) goal
  in map (skip xs 0) [0..pred (length xs)]

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima xs@(a:b:c:_) = (if b > a && b > c then [b] else []) ++ localMaxima (tail xs)
localMaxima (_:xs) = localMaxima xs
localMaxima [] = []

-- #3
histogram :: [Integer] -> String
histogram xs = let
  freqs = map (\n -> foldr (\x acc -> if n == x then succ acc else acc) 0 xs) [0..9]
  height = maximum freqs
  cols = map (\n -> replicate (height - n) ' ' ++ replicate n '*') freqs
  in concatMap (++ "\n")  (transpose cols) ++ "==========\n0123456789\n"
