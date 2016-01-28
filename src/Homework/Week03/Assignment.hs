module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

dropEveryOtherN :: Int -> [a] -> [a]
dropEveryOtherN 0 p = p 
dropEveryOtherN n [] = []
dropEveryOtherN n (x : xs) = [x] ++ dropEveryOtherN n (drop n xs)

stripHead :: [a] -> [[a]]
stripHead [] = []
stripHead p@(x:xs)
	| length xs == 0 = [p]
	| otherwise = [p] ++ stripHead xs

zipFunctions :: [[a]] -> [(Int, [a])]
zipFunctions p@(x:xs)= zip [0..((length p) - 1)] p 


buildList :: [(Int, [a])] -> [[a]]
buildList p@(x:xs)
	| length xs == 0  = [dropEveryOtherN (fst x) (snd x)]
	| otherwise = [dropEveryOtherN (fst x) (snd x)] ++ buildList xs

-- #1
skips :: [a] -> [[a]]
skips [] = []
skips p = buildList (zipFunctions (stripHead p))

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima = undefined

-- #3
histogram :: [Integer] -> String
histogram = undefined
