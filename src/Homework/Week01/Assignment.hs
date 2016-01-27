module Homework.Week01.Assignment where

-- #1
lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)

dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = lastDigit n : toDigitsRev (dropLastDigit n)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- #2
mapEveryOther :: (t -> t) -> [t] -> [t]
mapEveryOther _ []       = []
mapEveryOther _ [x]      = [x]
mapEveryOther f (x:y:ys) = x : f y : mapEveryOther f ys

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . mapEveryOther (*2) . reverse

-- #3
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

-- #4
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther $ toDigits n) `mod` 10 == 0

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
