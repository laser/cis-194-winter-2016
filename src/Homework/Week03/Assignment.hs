module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

-- #1
skips :: [a] -> [[a]]
skips l = map (\x -> map snd (filter (\(i, _) -> mod i x == 0) t)) [1..length l] where t = zip [1..] l

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima (_:[])      = []
localMaxima (x:y:[])    = []
localMaxima (x:y:z:l)   = let n = localMaxima $ y : z : l in if x < y && y > z then y : n else n

-- #3
histogram :: [Integer] -> String
histogram xs = undefined
