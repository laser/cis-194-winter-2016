module Homework.Week1 where

-------------
-- Ex 1-4  --
-------------

toDigits :: Integer -> [Integer]
toDigits = undefined

toDigitsRev :: Integer -> [Integer]
toDigitsRev = undefined

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = undefined

sumDigits :: [Integer] -> Integer
sumDigits = undefined

validate :: Integer -> Bool
validate = undefined

---------------------
-- Towers of Hanoi --
---------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined