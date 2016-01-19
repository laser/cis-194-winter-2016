module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

-- #1
skips :: [a] -> [[a]]
skips []  = []
skips xx@(_:xs) = xx : skips xs

-- #2
localMaxima :: [Integer] -> [Integer]
--localMaxima (a:b:c:xs)
--  |   a < b < c    = b : localMaxima (c : xs)
--  |   otherwise    = localMaxima (c : xs)                     
localMaxima _      = []


-- #3
histogram :: [Integer] -> String
histogram = undefined
