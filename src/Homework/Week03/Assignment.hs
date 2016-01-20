module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

getSingleWord :: [a] -> [Int] -> [a]
getSingleWord list =  map (\x -> list !! x) -- OMG!! I don't have style

-- #1
skips :: [a] -> [[a]]
skips [] = []
skips list = foldr (\x y -> getSingleWord list ( takeWhile ( < length list ) $ foldr (:) [] (iterate (+(x+1)) x)) : y) [] [0 .. (length list - 1)]

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs) = case y > x && y > z of
  True -> y : localMaxima (z:xs)
  otherwise -> localMaxima (y:z:xs)
localMaxima doesntmatch = []

-- #3
histogram :: [Integer] -> String
histogram = undefined
