module Homework.Week01.Assignment where

import Data.List (zipWith)

-- #1a
toDigits :: Integer -> [Integer]
toDigits n
   | n <= 0 = []
   | n > 0  = toDigits divBy10 ++ [val]
    where val      = n `rem` 10
          divBy10  = n `div` 10
revList :: [Integer] -> [Integer]
revList = foldl (flip (:)) []

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
   | n <= 0    = []
   | n > 0     = (revList . toDigits) n


-- #2
-- rev . doubleEveryOther . rev
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = revList . zipWith (*) multiplier . revList
  where multiplier = concat $ replicate 20 [1,2]

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
