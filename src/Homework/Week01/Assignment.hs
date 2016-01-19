module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    =  []
  | otherwise =  n `mod` 10 : toDigitsRev (n `div` 10)

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherRev (reverse xs))

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev []              = []
doubleEveryOtherRev [x]             = [ x ]
doubleEveryOtherRev [x, y]          = [ x, 2 * y ]
doubleEveryOtherRev (x : y : zs)    = [ x, 2 * y ] ++ doubleEveryOtherRev (zs)


-- #3
sumDigits :: [Integer] -> Integer
sumDigits []       = 0
sumDigits (x : ys) = sumDigit x + sumDigits ys

sumDigit :: Integer -> Integer
sumDigit n
  | n <= 0    = 0
  | n < 10    = n
  | otherwise = 1 + sumDigit (n - 10)


-- #4
validate :: Integer -> Bool
validate n = sumDigits( doubleEveryOther ( toDigits n )) `mod` 10 == 0

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 source target temp = []
hanoi 1 source target temp = [(source, target)]
hanoi n source target temp = (hanoi (n-1) source temp target) ++ [(source, target)] ++ (hanoi (n-1) temp target source)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
