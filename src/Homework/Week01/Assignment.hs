module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)
-- toDigits = undefined

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x
    | x < 0 = []
    | otherwise = (x `mod` 10) : toDigitsRev (x `div` 10) 

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherFromLeft(reverse(xs)))

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []
doubleEveryOtherFromLeft (x:[]) = [x]
doubleEveryOtherFromLeft (x:(y:xs)) = x : (2*y) : doubleEveryOtherFromLeft(xs)

-- #3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:[]) = x
sumDigits (x:xs) = sumDigits(toDigits(x)) + sumDigits(xs)

-- #4
validate :: Integer -> Bool
validate x = sumDigits(doubleEveryOther(toDigits(x))) `mod` 10 == 0

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
