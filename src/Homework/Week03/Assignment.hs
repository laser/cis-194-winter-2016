module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where


  -- fucntion to get every nth element of the list
  -- first argument is the list
  -- second argument is the n
  -- third argument is also n and it is preserved for entire recursion
getEveryNthElement :: [a] -> Int -> Int -> [a]
getEveryNthElement list x y
  | x > (length list) = []
  | otherwise = (list !! (x-1)) : getEveryNthElement list (x+y) y



skipsEveryNthElement :: [a] -> Int -> [[a]]
skipsEveryNthElement []  _ = []
skipsEveryNthElement nonEmptyList num
  | num > length nonEmptyList = []
  | otherwise = getEveryNthElement nonEmptyList num num : skipsEveryNthElement nonEmptyList (num+1)

-- #1
skips :: [a] -> [[a]]
skips list = skipsEveryNthElement list 1

isElementGreater :: Integer -> Integer -> Integer -> Bool
isElementGreater elementToBeTested x y
  | elementToBeTested > x && elementToBeTested > y = True
  | otherwise = False


localMaximaAtIndex :: [Integer] -> Int -> [Integer]
localMaximaAtIndex list index
  | index >= (length list - 1) = []
  | keep = (list !! index) : localMaximaAtIndex list (index+1)
  | otherwise =  localMaximaAtIndex list (index+1)
  where keep = isElementGreater (list !! index) (list !! (index-1)) (list !! (index+1))

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima list
  | length list <=2 = []
  | otherwise = localMaximaAtIndex list 1

-- return the number of elements in the input list and the first element
-- not being used
count :: [Integer] -> (Int, Integer)
count [] = (0, 0)
count list = (length list, head list)

freqOfSingleElement :: [Integer] -> Integer -> Integer
freqOfSingleElement [] _ = 0
freqOfSingleElement list x
  | head list == x = 1+freqOfSingleElement (tail list) x
  | otherwise = freqOfSingleElement (tail list) x

freqUptoNine :: [Integer] -> [Integer]
freqUptoNine list =  map (freqOfSingleElement list) [0..9]


printOneRow :: [Integer] -> Integer -> String
printOneRow [] _ = ""
printOneRow (x:xs) n
  | x >= n = "*" ++ (printOneRow xs n)
  | otherwise = " " ++ (printOneRow xs n)


maxFreq :: [Integer] -> Integer
maxFreq list = maximum (freqUptoNine list)

-- #3
histogram :: [Integer] -> String
histogram list =
  let freqAtIndex = freqUptoNine list
      max = maxFreq list
      output = unlines (map (printOneRow freqAtIndex) [max,max-1..1]) ++ "==========\n0123456789\n"
  in output
