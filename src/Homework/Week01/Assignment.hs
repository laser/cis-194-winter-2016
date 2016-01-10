module Homework.Week01.Assignment where

-- #1a
toDigits :: Integer -> [Integer]
toDigits n
  | n < 0 || n == 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 0 || n == 0 = []
  | otherwise = [n `mod` 10] ++ toDigitsRev (n `div` 10)


-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:(y:[])) = [2* x, y]
doubleEveryOther (x:(y:zs))
  | isEven = [2*x, y] ++ doubleEveryOther (zs)
  | otherwise = [x, 2*y] ++ doubleEveryOther (zs)
  where isEven = (length (x:(y:zs)) `mod` 2 == 0)



-- #3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:zs) = sum (toDigits (x)) + sumDigits (zs)

-- #4
validate :: Integer -> Bool
validate n = remainder == 0
  where remainder = sumDigits(doubleEveryOther(toDigits(n))) `mod` 10

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi 1 a b c = [(a, b)]
hanoi n a b c =  hanoi (n-1) a c b ++ hanoi 1 a b c ++ hanoi (n-1) c b a


hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
