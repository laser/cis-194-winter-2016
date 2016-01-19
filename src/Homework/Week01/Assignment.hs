module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
  | n < 0     = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n
  | n < 0     = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther (x:[])   = [x]
doubleEveryOther (x:xs)   = (reverse . doubleEveryOtherFromLeft . reverse) (x:xs)

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft []       = []
doubleEveryOtherFromLeft [x]      = [x]
doubleEveryOtherFromLeft (x:y:xs) = x : (y*2) : doubleEveryOtherFromLeft xs

-- #3
sumDigits :: [Integer] -> Integer
sumDigits []      = 0
sumDigits [x]
  | (x < 10)  = x
  | otherwise = (sumDigits . toDigits) x
sumDigits (x:xs)  = sumDigits [x] + sumDigits xs

-- #4
validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0

-- #5
-- type Peg = String
-- type Move = (Peg, Peg)
--
-- hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- hanoi = undefined
--
-- hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
-- hanoi4 = undefined
