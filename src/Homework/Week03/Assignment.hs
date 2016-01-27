module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

import           Data.List (find, sort, span)

-- #1
skips :: [a] -> [[a]]
skips xs = map (\n -> foldr (\(m, y) a -> if m `mod` n == 0 then y : a else a) [] $ zip [1..] xs) [1..length xs]

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima (i:j:k:ks) = let z = localMaxima (j:k:ks) in
                         if (i < j && k < j) then j:z else z
localMaxima _ = []

-- #3
histogram :: [Integer] -> String
histogram = (++"==========\n0123456789\n") . rows . df
  where
    dot n = if n > 0 then '*' else ' '
    rows ns = maybe "" (const $ rows (map (+(-1)) ns) ++ (map dot ns ++ "\n")) (find (>0) ns)
    df = go 0 . sort
      where go n ns
              | n == 10 = []
              | 1>0 = let (l,r) = span (==n) ns in
                      length l:go (n+1) r
