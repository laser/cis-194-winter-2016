module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

-- #1
skips :: [a] -> [[a]]
skips xs = map (\n -> skip n 0 xs) [0..length xs - 1]
  where
    skip _ _ [] = []
    skip goal accum (y:ys)
      | accum == goal = y : skip goal 0 ys
      | accum < goal  = skip goal (accum + 1) ys
      | otherwise = error "Bad arg value: 'accum' should never be greater than 'goal'"

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima xs@(a:b:c:_) = (if b > a && b > c then [b] else []) ++ localMaxima (tail xs)
localMaxima (_:xs) = localMaxima xs
localMaxima [] = []

-- #3
histogram :: [Integer] -> String
histogram xs = let
  increment y ys = take y ys ++ [ys !! y + 1] ++ drop (y + 1) ys
  counts = foldr increment (replicate 10 0) (map fromIntegral xs)
  rows = foldr1 max counts
  mkRow row = map (\col -> if counts !! col >= row then '*' else ' ') [0..9] ++ "\n"
  in concat (map mkRow [rows,rows-1..1]) ++ "==========\n0123456789\n"
