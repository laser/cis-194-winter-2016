module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

import Data.List.Split (divvy)

-- #1
skips :: [a] -> [[a]]
skips xs = map (`everyNth` xs) [1..size]
  where size = length xs

everyNth :: Int -> [a] -> [a]
everyNth n xs = case drop (n - 1) xs of
  (y:ys) -> y : everyNth n ys
  []     -> []

-- #2
isLocalMaximum :: Ord a => [a] -> Bool
isLocalMaximum [a, x, b] = x > a && x > b
isLocalMaximum _         = error "must be given a triplet"

localMaxima :: [Integer] -> [Integer]
localMaxima = map (!! 1) . filter isLocalMaximum . divvy 3 1

-- #3
type Bin = (Integer, Int)

digitBin :: [Integer] -> [Bin]
digitBin xs = map assocPair [0..9]
  where assocPair n = (n, length $ filter (== n) xs)

barLine :: [Bin] -> Int -> String
barLine bins n = concatMap (barMarker . (>= n) . snd) bins
  where barMarker True  = "*"
        barMarker False = " "

histogram :: [Integer] -> String
histogram xs = unlines allRows
  where bins       = digitBin xs
        largestBin = maximum $ map snd bins
        bars       = reverse $ map (barLine bins) [1..largestBin]
        allRows    = bars ++ ["==========", "0123456789"]
