module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

-- #1
skips :: [a] -> [[a]]
skips xs = map snd (map unzip (map (\skipPosition -> filter(\(position, element) -> position `mod` skipPosition == 0) zipWithPosition) positions))
    where zipWithPosition = zip [1..] xs
          positions = fst (unzip zipWithPosition)

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (x1:[]) = []
localMaxima (x1:x2:[]) = []
localMaxima (x1:x2:x3:xs) = case x1 < x2 && x2 > x3 of
                              True -> x2 : localMaxima(x2:x3:xs)
                              False -> localMaxima(x2:x3:xs)

-- #3
histogram :: [Integer] -> String
histogram = undefined
