module Homework.Week01.Assignment where
-- #1a
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = (n `mod` 10) : toDigitsRev(n `div` 10)

-- #2
doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft (x : y : z) = x : (2 * y) : doubleEveryOtherFromLeft z
doubleEveryOtherFromLeft list = list

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list  = reverse $ doubleEveryOtherFromLeft (reverse list)

-- #3
sumDigits :: [Integer] -> Integer
sumDigits list = sum $ concatMap toDigits list

-- #4
validate :: Integer -> Bool
validate = (== 0 ) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi move a b c = hanoi (move - 1) a c b ++ [(a,b)] ++ hanoi (move - 1) c b a

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
