module Homework.Week01.Assignment where

-- helper functions for 1a
parseDigitsToArray :: Integer -> [Integer]
parseDigitsToArray n
  | n < 10 = [n]
  | otherwise = parseDigitsToArray(n `div` 10) ++ [n `mod` 10]

-- #1a
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = parseDigitsToArray n

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

returnADoubleValueIfIndexIsEvenElseReturnSameValueFromList :: Int -> [Int] -> Int
returnADoubleValueIfIndexIsEvenElseReturnSameValueFromList n list
  | n `mod` 2 == 1 = 2 * (drop n list !! 0)
  | otherwise = drop n list !! 0

-- #2
doubleEveryOtherFromLeft :: [Int] -> [Int]
doubleEveryOtherFromLeft list = foldr (\x y -> ((returnADoubleValueIfIndexIsEvenElseReturnSameValueFromList x) list) : y) [] [0 .. length list - 1]

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther list = reverse (doubleEveryOtherFromLeft (reverse list))

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
