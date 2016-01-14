module Homework.Week01.Assignment where
import Data.Char

-- #1a
toDigits :: Integer -> [Integer]
toDigits x
	| x <= 0 = []
	| otherwise = map toInteger . map digitToInt . show $ x

-- #1b
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- #2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . deol . reverse

deol :: [Integer] -> [Integer]
deol [] = []
deol (x:[]) = [x] 
deol (x:y:xs) = [x] ++ [y * 2] ++ deol xs

-- #3
sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

-- #4()
validate :: Integer -> Bool
validate =  (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

-- #5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
