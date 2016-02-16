module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits i
    | i <= 0            = []
    | i `mod` 10 == i   = [i]
    | otherwise         = toDigits (i `div` 10) ++ [(i `mod` 10)]

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev i = reverse (toDigits i)

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []         = []
doubleEveryOther (x:[])     = [x]
--doubleEveryOther (x:y:[])   = [x * 2, y]
--doubleEveryOther xs         = doubleEveryOther (take (length xs - 2) xs)
--                              ++ doubleEveryOther (drop (length xs - 2) xs) 
doubleEveryOther xs
    | length xs `mod` 2 == 0    = zipWith (*) xs (cycle [2, 1])
    | otherwise                 = zipWith (*) xs (cycle [1, 2, 1])
--    | otherwise                 = take 1 xs !! 0 : doubleEveryOther (drop 1 xs)

-- #3
sumDigits :: [Integer] -> Integer
sumDigits xs = sum (concatMap toDigits xs)

-- #4
validate :: Integer -> Bool
validate x = x > 0 && sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
