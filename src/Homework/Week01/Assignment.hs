module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits n
     | n <= 0     = []
     | otherwise  = toDigits(div n 10) ++ [mod n 10]

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = zipWith (*) xs $ cycle ys
    where ys = if odd (length xs) 
                 then [1,2] 
                 else [2,1]

-- #3
sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ concatMap toDigits xs

-- #4
validate :: Integer -> Bool
validate n = mod checksum 10 == 0
    where checksum = sumDigits . doubleEveryOther $ toDigits n

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
