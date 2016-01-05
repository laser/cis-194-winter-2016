module Homework.Week01.Assignment where

import Data.Char
-- #1a
toDigits :: Integer -> [Integer]
toDigits n
  |  n <= 0     = []
  |  otherwise  = map (toInteger . digitToInt) $  show n 

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther  = reverse . map doubleEveryOtherNumber . zip [0..] . reverse 
                    where doubleEveryOtherNumber (first,second) = if odd first then second * 2 else second 

-- #3
sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits 

-- #4
validate :: Integer -> Bool
validate = (\n -> mod (sumDigits $ toDigits n) 10 /=  0)

-- #5
type Peg = String
type Move = (Peg, Peg)

__ = ""

--1. move n-1 discs from a to b using c as temporary storage
--2. move the top disc from a to c
--3. move n-1 discs from b to c using a as temporary storage.

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _          = []
hanoi 1 begin end _    = (begin,end) : hanoi 0 __ __ __
hanoi n begin end temp = hanoi (n-1) begin end   temp ++
                         hanoi  1    begin temp  end  ++
                         hanoi (n-1) temp  begin end
                                  

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _  = []
hanoi4 n _ _ _ _  = []
