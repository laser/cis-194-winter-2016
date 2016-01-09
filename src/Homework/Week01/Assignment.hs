module Homework.Week01.Assignment where
import Data.Char

-- #1a
toDigits :: Integer -> [Integer]
toDigits x
	| x <= 0 = []
	| otherwise = map toInteger (map digitToInt (show x))

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []

-- #3
sumDigits :: [Integer] -> Integer
sumDigits x = sum (concat (map toDigits x))

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
