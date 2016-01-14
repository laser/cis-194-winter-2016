module Homework.Week01.Assignment where
import Data.Char
-- #1a
toDigits :: Integer -> [Integer]
toDigits n
  |n <= 0 = []
  |otherwise = map toInteger (map digitToInt (show n))

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' [x] = [x]
doubleEveryOther' (x:y:xs) = x:(2*y):(doubleEveryOther' xs)
doubleEveryOther = reverse . doubleEveryOther' . reverse
-- #3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum(toDigits x) + (sumDigits xs)

-- #4
validate :: Integer -> Bool
validate = (==0) .  (`mod` 10) . sumDigits . doubleEveryOther . toDigits

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
