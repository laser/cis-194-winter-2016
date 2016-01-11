module Homework.Week01.Assignment where

-- #1a
-- helper functions

parseDigitsToArray :: Integer -> [Integer]
parseDigitsToArray n
  | n < 10 = [n]
  | otherwise = parseDigitsToArray(n `div` 10) ++ [n `mod` 10]

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = parseDigitsToArray n

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = map(\x -> x * 2) list

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
