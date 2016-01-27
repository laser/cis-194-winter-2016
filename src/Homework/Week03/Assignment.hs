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
skips' n xs = skipsEvery n xs : skips' (n + 1) (tail xs)

skipsEvery :: Int -> [a] -> [a]
skipsEvery _ [] = []
skipsEvery n (x:xs) = x : skipsEvery n (drop n xs)

-- #2
localMaxima :: [Int] -> [Int]
localMaxima (a:b:c:xs) = (if (a < b && c < b) then [b] else []) ++ localMaxima (b:c:xs)
localMaxima _ = []

-- #3
histogram :: [Int] -> String
histogram xs = unlines $ histogramRows xs ++ ["==========", "0123456789"]

histogramRows :: [Int] -> [String]
histogramRows [] = []
histogramRows ints = histogramRows' (maximum counts) counts
  where counts = histogramCounts ints

histogramCounts :: [Int] -> [Int]
histogramCounts ints = map (\ i -> frequency i ints) [0..9]

frequency :: Int -> [Int] -> Int
frequency _ [] = 0
frequency i ints = frequency i (tail ints) + if (i == (head ints)) then 1 else 0

histogramRows' :: Int -> [Int] -> [String]
histogramRows' 0 _ = []
histogramRows' n ints = histogramRow n ints : histogramRows' (n - 1) ints

histogramRow :: Int -> [Int] -> String
histogramRow n counts = map (\ c -> if (c >= n) then '*' else ' ') counts
