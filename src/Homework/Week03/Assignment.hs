module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

-- #1
skips :: [a] -> [[a]]
skips xs = map snd (map unzip (map  (loopNumberOFElementsTimes zipWithIdx) zipWithIdx))
    where zipWithIdx = zip [1..] xs
          loopNumberOFElementsTimes zipWithIdx (loopIdx, _) = filter (indexMultipleOf $ loopIdx) zipWithIdx
          indexMultipleOf multiple (idx, element) =  idx `mod` multiple == 0
-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima = undefined

-- #3
histogram :: [Integer] -> String
histogram = undefined
