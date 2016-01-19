module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

-- #1
skips :: [a] -> [[a]]
skips xss = takeWhile (\x -> case x of
                        [] -> False
                        _  -> True ) $ map (doSkip xss) [1..]
            where doSkip xs n = map fst $ filter (\ (e,idx) -> idx `mod` n == 0 ) $ zip xs [1..]


-- #2
localMaxima :: [Integer] -> [Integer]
--localMaxima (a:b:c:xs)
--  |   a < b < c    = b : localMaxima (c : xs)
--  |   otherwise    = localMaxima (c : xs)                     
localMaxima _      = []


-- #3
histogram :: [Integer] -> String
histogram = undefined
