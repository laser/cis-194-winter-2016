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
localMaxima (x:xs)
	| length xs == 0 = []
localMaxima p@(x:y:xs)
	| length p == 1 = []
	| length xs == 0 = []
	| y > x && y > (head xs) = [y] ++ localMaxima (xs)
	| otherwise = localMaxima ([y] ++ xs)

-- #3
histogram :: [Integer] -> String
histogram p = "==========\n0123456789\n"
