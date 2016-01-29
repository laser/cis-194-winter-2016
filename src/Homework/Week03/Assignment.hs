module Homework.Week03.Assignment (
  takeEvery,
  addBase,
  addEmptyCols,
  skips,
  localMaxima,
  histogram
) where

import           Data.List
import           Debug.Trace

-- #1
takeEvery :: Int -> [a] -> [a]
takeEvery n l = case drop (n-1) l of
  []     -> []
  (x:xs) -> x : takeEvery n xs

skips :: [a] -> [[a]]
skips l = zipWith takeEvery [1..] (replicate (length l) l)

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima l@(x:y:z:_) = if (x < y && y > z)
  then y : localMaxima (drop 1 l)
  else localMaxima (drop 1 l)
localMaxima _ = []

-- #3
addBase :: [String] -> String
addBase l = intercalate "\n" (l ++ ["==========", "0123456789\n"])

addEmptyCols :: [Integer] -> [[Integer]] -> [[Integer]]
addEmptyCols []        _        = []
addEmptyCols ns@(a:as) []       = replicate (length ns) []
addEmptyCols (a:as)    l@(b:bs) = if a `elem` b
                                    then b   : addEmptyCols as bs
                                    else []  : addEmptyCols as l
histogram :: [Integer] -> String
histogram l =
  let gsl  = addEmptyCols [0..9] (group (sort l))
      ls   = map length gsl
      top  = maximum ls
      rows = map (\ x -> (replicate x "*") ++ (replicate (top - x) " ")) ls
      cols = map concat (reverse (transpose rows))
  in addBase cols
