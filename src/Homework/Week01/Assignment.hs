module Homework.Week01.Assignment where

import Data.Char

-- #1a
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = map toInteger (map digitToInt (show n))

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse (reversedDouble (reverse n))

reversedDouble :: [Integer] -> [Integer]
reversedDouble [] = []
reversedDouble [x] = [x]
reversedDouble (x:y:z) = x : (2 * y) : (reversedDouble z)

-- #3
sumDigits :: [Integer] -> Integer
sumDigits n = foldr (+) 0 (map sumDigit n)

sumDigit :: Integer -> Integer
sumDigit n = foldr (+) 0 (toDigits n)

-- #4
validate :: Integer -> Bool
validate n
  | (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0 = True
  | otherwise = False

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
