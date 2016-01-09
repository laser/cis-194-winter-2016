module Homework.Week01.Assignment where

import Data.List (zipWith)

div10 :: Integral a => a -> a
div10 n = n `div` 10

rem10 :: Integral a => a -> a
rem10 n
  | n <= 0 = 0
  | n > 0  = n `rem` 10


-- #1a
toDigits :: Integer -> [Integer]
toDigits n
   | n <= 0 = []
   | n > 0  = toDigits divBy10 ++ [val]
    where val      = n `rem` 10
          divBy10  = n `div` 10

-- helper
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

-- helper

-- destructVal 12 -> [1,2]
-- destructVal 1  -> [1]
destructVal :: Integer ->[Integer]
destructVal n
  | n <= 0      =  [0]
  | n < 10      = [rem10 n]
  | n == 10     = [1,0]
  | n > 10      = div10 n : [rem10 n]



sumDigits :: [Integer] -> Integer
sumDigits xs   = sum  (concatMap destructVal xs)

-- #4

validate :: Integer -> Bool
validate n = validate' n  == 0 where
  validate'  = rem10 . sumDigits . doubleEveryOther . toDigits

-- -- #5
-- type Peg = String
-- type Move = (Peg, Peg)

-- hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- hanoi = undefined
--
-- hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
-- hanoi4 = undefined
