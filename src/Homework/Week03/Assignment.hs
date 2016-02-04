module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

import Data.List
--import Data.Map (fromList, fromListWith, toList, unionWith)

-- #1
skipNth :: Int -> [a] -> [a]
skipNth n xs = map fst (filter (\ (x, idx) -> (idx `mod` (n) == 0)) (xs `zip` [1..]))

skips :: [a] -> [[a]]
skips [] = []
skips xs =
         let repeatWithIndex = (replicate (length xs) xs) `zip` [1..]
         in map (\(x,idx) -> (skipNth idx x)) repeatWithIndex
-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs)
    | a<b && b>c = [b] ++ localMaxima(c:xs)
    | otherwise = localMaxima(b:c:xs)
localMaxima _ = []

-- #3
{-
frequency :: [Integer] -> [(Integer, Integer)]
frequency xs = toList (fromListWith (+) [(x,1) | x <- xs])

get_max_from_lst xs = head (map fst $ filter ((== m) . snd) xs)
    where m = maximum $ map snd xs

filler :: [(Integer, Integer)] -> [(Integer, Integer)]
filler xs =
          let blank = fromList([0..9] `zip` (repeat 0))
          in toList (unionWith (+) (fromList xs) blank)


format :: [(Integer, Integer)] -> [String]
format xs =
           let size = get_max_from_lst xs
           in
           (map (\(x,y) -> ((show x) ++ "=" ++ (replicate (fromIntegral(y)) '*') ++ (replicate (fromIntegral(size) - (fromIntegral(y))) ' '))) xs) ++
           (replicate (fromIntegral size) "\n")

-}

histogram :: [Integer] -> String
histogram = undefined
