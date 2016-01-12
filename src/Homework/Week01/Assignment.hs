module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
--    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]
    | otherwise = reverse $ toDigitsRev n

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0    = []
    | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []             = []
doubleEveryOther xs             = reverse $ doubleEveryOtherLeftToRight $ reverse xs

doubleEveryOtherLeftToRight :: [Integer] -> [Integer]
doubleEveryOtherLeftToRight []              = []
doubleEveryOtherLeftToRight [x]             = [x]
doubleEveryOtherLeftToRight (x : y : zs)    = x : (2 * y) : doubleEveryOtherLeftToRight zs

-- #3
sumDigits :: [Integer] -> Integer
sumDigits []        = 0
sumDigits [x]       = x
sumDigits (x : xs)  = (sumDigits $ toDigits x) + sumDigits xs

-- #4
validate :: Integer -> Bool
validate n
    | (sumDigits $ doubleEveryOther $ toDigits n) `mod` 10 == 0 = True
    | otherwise                                                 = False


-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b c = [(a, b)]

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined

