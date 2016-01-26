module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

-- #1
skips :: [a] -> [[a]]
skips xs = map snd (map unzip (map (\(loopIdx, _) -> filter(\(idx, element) -> idx `mod` loopIdx == 0) zipWithIdx) zipWithIdx))
    where zipWithIdx = zip [1..] xs

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima = undefined

-- #3
histogram :: [Integer] -> String
histogram = undefined
