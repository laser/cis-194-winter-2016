module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits number = reverse . toDigitsRev

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev number
  | number < 1 = []
  | otherwise =
      let (remain, digit) = divMod number 10
      in digit : toDigitsRev(remain)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery size list = (take size list) : splitEvery size (drop size list)

doubleFirst :: [Integer] -> [Integer]
doubleFirst (first : remain) = (first * 2) : remain

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = concatMap doubleFirst . splitEvery 2

-- #3
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

-- #4
validate :: Integer -> Bool
validate = (== 0) . (\x -> mod x 10) . sumDigits . doubleEveryOther . toDigits

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
