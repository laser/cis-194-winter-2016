module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

getTheRightStringBaseOnTheStartingPoint :: [(a,Int)] -> Int -> [a]
getTheRightStringBaseOnTheStartingPoint listWithIndices startingPoint
  = map fst (filter (\x -> mod (snd x) startingPoint == 0) listWithIndices )

-- #1
skips :: [a] -> [[a]]
skips [] = []
skips list
  = map (getTheRightStringBaseOnTheStartingPoint (zip list [1..])) [1 .. (length list)]

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs) = case y > x && y > z of
  True -> y : localMaxima (z:xs)
  otherwise -> localMaxima (y:z:xs)
localMaxima doesntmatch = []

-- #3
histogram :: [Integer] -> String
histogram = undefined
