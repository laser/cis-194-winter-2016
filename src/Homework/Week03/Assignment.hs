module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

-- #1
skips :: [a] -> [[a]]
skips xs = map f [1..length xs]
    where f n = every n xs

every n xs = case drop (n - 1) xs of
    y : ys -> y : every n ys
    []     -> []

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima (x : y : z : zs)
    | y > x && y > z = y : localMaxima (z : zs)
    | otherwise      = localMaxima (y : z : zs)
localMaxima _ = []

-- #3
histogram :: [Integer] -> String
histogram = unlines . reverse . rows . frequencies

frequencies :: [Integer] -> [Integer]
frequencies xs = map count [0..9]
    where count n = toInteger . length . filter ( == n) $ xs

rows :: [Integer] -> [String]
rows frequencies = binTags : map ( `row` frequencies) [1..rowCount]
    where rowCount = maximum frequencies

row :: Integer -> [Integer] -> String
row n frequencies = map plot frequencies
    where plot frequency = if n <= frequency then '*' else ' '

binTags = "==========\n0123456789"
