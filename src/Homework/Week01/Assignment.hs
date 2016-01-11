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
doubleEveryOther xs = reverse (doubleEveryOther1(reverse(xs)))

doubleEveryOther1 :: [Integer] -> [Integer]
doubleEveryOther1 [] = []
doubleEveryOther1 (x:[]) = [x]
doubleEveryOther1 (x:(y:[])) = x : (2*y) : []
doubleEveryOther1 (x:(y:xs)) = x : (2*y) : doubleEveryOther1(xs)

-- #3
sumDigits :: [Integer] -> Integer
sumDigits = undefined

-- #4
validate :: Integer -> Bool
validate = undefined

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
