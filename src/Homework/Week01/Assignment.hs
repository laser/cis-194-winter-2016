module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0    = []
    | otherwise = (x `mod` 10) : toDigitsRev (x `div` 10)

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherLeftToRight . reverse

doubleEveryOtherLeftToRight :: [Integer] -> [Integer]
doubleEveryOtherLeftToRight []              = []
doubleEveryOtherLeftToRight [x]             = [x]
doubleEveryOtherLeftToRight (x : y : zs)    = x : (2 * y) : doubleEveryOtherLeftToRight zs

-- #3
sumDigits :: [Integer] -> Integer
sumDigits []        = 0
sumDigits [x]       = x
sumDigits (x : xs)  = (sumDigits . toDigits $ x) + sumDigits xs

-- #4
validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits $ x) `mod` 10 == 0


-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi x a b c = (hanoi (x - 1) a c b) ++ [(a, b)] ++ (hanoi (x - 1) c b a)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined

