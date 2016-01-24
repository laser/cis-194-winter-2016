module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

import Data.List

-- #1
skips :: [a] -> [[a]]
skips [] = []
skips xs = skips' 0 xs

skips' :: Int -> [a] -> [[a]]
skips' _ [] = []
skips' n xs = (skipsEvery n xs) : skips' (n + 1) (drop 1 xs)

skipsEvery :: Int -> [a] -> [a]
skipsEvery _ [] = []
skipsEvery n (x:xs) = x : skipsEvery n (drop n xs)

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima = undefined

-- #3
histogram :: [Integer] -> String
histogram = undefined
