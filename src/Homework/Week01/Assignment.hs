module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev number
  | number < 1 = []
  | otherwise =
      let (remain, digit) = divMod number 10
      in digit : toDigitsRev(remain)

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (first : second : remain) = (first * 2) : second : doubleEveryOther(remain)

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
hanoi 0 from to storage = []
hanoi 1 from to storage = [(from, to)]
hanoi disks from to storage = (hanoi (disks-1) from storage to) ++ [(from, to)] ++ (hanoi (disks-1) storage to from)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
