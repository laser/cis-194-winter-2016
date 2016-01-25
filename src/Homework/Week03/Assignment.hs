
module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

import Data.List (sort,group)

-- #1
skips :: [a] -> [[a]]
skips xs = zipWith everyNth (repeat xs) [1..length(xs)]
    where everyNth xs n = map fst $ filter (\(_,i) -> i==n) (zip xs $ cycle [1..n])

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima xs = case xs of
    (x:y:z:[]) -> if  x < y && y > z then [y] else []
    (x:y:z:r)  -> localMaxima [x,y,z] ++ localMaxima (y:z:r)
    _          -> []


-- #3
histogram :: [Integer] -> String 
histogram xs = unlines rows ++ "==========\n0123456789\n"
    where rows = zipWith histline (repeat freqs) (reverse [1..maxFreq])
          histline freqs row = foldl (\ acc n -> acc ++ if lookup n freqs >= Just row then "*" else " ") "" [0..9]
          freqs = map (\ xs -> (head xs, length xs)) (group $ sort xs)
          maxFreq = maximum (map snd freqs ++ [0])
