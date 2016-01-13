module Homework.Week01.Assignment where
-- #1a
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = (n `mod` 10) : toDigitsRev(n `div` 10)

-- #1b
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

--first attempt
-- returnADoubleValueIfIndexIsEvenElseReturnSameValueFromList :: Int -> [Integer] -> Integer
-- returnADoubleValueIfIndexIsEvenElseReturnSameValueFromList n list
--     | n `mod` 2 == 1 = 2 * (head $ drop n list)
--     | otherwise = head $ drop n list
--
-- -- #2
-- doubleEveryOtherFromLeft :: [Integer] -> (Int -> [Integer] -> Integer) -> [Integer]
-- doubleEveryOtherFromLeft list accFunc = foldr (\x y -> accFunc x list : y) [] [0 .. length list - 1]
--
-- doubleEveryOther :: [Integer] -> [Integer]
-- doubleEveryOther list = reverse $ doubleEveryOtherFromLeft (reverse list) returnADoubleValueIfIndexIsEvenElseReturnSameValueFromList

--second attempt
getListWithIndices :: [Integer] -> [(Integer, Integer)]
getListWithIndices list = zip list [0 .. toInteger $ length list - 1]

returnIntegerTwoIfItsOdd :: Integer -> Integer
returnIntegerTwoIfItsOdd n
  | odd n = 2
  | otherwise = 1

doubleIfIndexIsEven :: (Integer, Integer) -> Integer
doubleIfIndexIsEven pair
  = let (digit, index) = pair
    in digit * returnIntegerTwoIfItsOdd index


doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft list = map doubleIfIndexIsEven (getListWithIndices list)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = reverse $ doubleEveryOtherFromLeft (reverse list)

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
