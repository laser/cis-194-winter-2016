module Homework.Week03.Assignment (
  skips
 , pick
 , localMaxima
 , histogram
) where

import Data.List (sort, group, intercalate)
import Data.Maybe


pick :: Int -> [a] -> [a]
pick _ [] = []
pick n as = frst ++ pick n ( drop n as )
    where frst  = drop (n-1) (take n as)

helper :: [Int] -> [a] -> [[a]]
helper [] _ = []
helper nts  xs = pick (head nts) xs : helper (tail nts) xs

skips :: [a] -> [[a]]
skips xs = helper [1..(length xs)] xs


localMaxima :: [Integer] -> [Integer]
localMaxima []  = []
localMaxima (x : as@(y : z :rest))
  | (x < y && y >= z) = y : localMaxima as
  | otherwise   = localMaxima as
localMaxima _  = []

freqs ::   [Int] -> (Int, Int)
freqs (val : xs) = (val , (length xs + 1))
  -- let len = (length  xs) + 1
  -- in (val , len)


tups :: [Int] -> [(Int, Int)]
tups = map freqs . group . sort

fromm :: [Int] -> [Int]
fromm xs =
  let tps = tups xs
  --in map (fromMaybe 0 ) $ map (flip lookup tps) [0..9]
  in map (fromMaybe 0  . flip lookup tps) [0..9]

showIt:: Int -> Char
showIt x
   | (x > 0) = '*'
   | otherwise = ' '

toChars :: [[Int]] -> [[Char]]
toChars [] = [[]]
toChars (x : xs) = map showIt x : toChars xs

histogram :: [Int] -> String
histogram m =
   let trans = fromm m
       mx    = (maximum trans) - (1 :: Int)
       zod   = map (flip (\val -> map (\x -> x - val)) $ trans) [0..mx]
       endd  = "==========\n0123456789\n"
       final = drop 1 $ reverse (toChars zod) ++ [endd]
   in intercalate "\n" final
