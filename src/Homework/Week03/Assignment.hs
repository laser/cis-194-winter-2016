module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

-- #1
skips :: [a] -> [[a]]
skips = reverse . foldr step [] . reverse . tails'
    where step xs accu = (take1stAndEveryNth n xs) : accu
            where n = length accu + 1

-- Data.List.tails sans an [] at the end
tails' :: [a] -> [[a]]
tails' xs@(_ : ys) = xs : tails' ys
tails' _           = []

--  n > 0
take1stAndEveryNth :: Int -> [a] -> [a]
take1stAndEveryNth _ []       = []
take1stAndEveryNth n (x : xs) = x : take1stAndEveryNth n ys
    where ys = drop (n - 1) xs

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima []          = []
localMaxima xs@(_ : ys) = case localMaximum . take 3 $ xs of
    Just it -> it : localMaxima ys
    Nothing -> localMaxima ys

localMaximum :: [Integer] -> Maybe Integer
localMaximum (x : y : [z])
    | y > x && y > z = Just y
    | otherwise      = Nothing
localMaximum _       = Nothing

-- #3
histogram :: [Integer] -> String
histogram = unlines . reverse . rows . frequencies

frequencies :: [Integer] -> [Integer]
frequencies xs = map count [0..9]
    where count n = toInteger . length . filter (n == ) $ xs

rows :: [Integer] -> [String]
rows frequencies = binTags : map ( `row` frequencies) [1..rowCount]
    where rowCount = maximum frequencies

row :: Integer -> [Integer] -> String
row n frequencies = map plot frequencies
    where plot frequency = if n <= frequency then '*' else ' '

binTags = "==========\n0123456789"
