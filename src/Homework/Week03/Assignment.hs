module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

getTheRightElementBaseOnTheStartingPoint :: [(a,Int)] -> Int -> [a]
getTheRightElementBaseOnTheStartingPoint listWithIndices startingPoint
  = map fst (filter (\x -> mod (snd x) startingPoint == 0) listWithIndices )

-- #1
skips :: [a] -> [[a]]
skips [] = []
skips list
  = map (getTheRightElementBaseOnTheStartingPoint (zip list [1..])) [1 .. (length list)]

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs) = if y > x && y > z
                         then y : localMaxima (z:xs)
                         else localMaxima (y:z:xs)
localMaxima doesntmatch = []

-- #3
getCountList :: [Integer] -> [Int]
getCountList list = [length $ filter (==0) list
                    ,length $ filter (==1) list
                    ,length $ filter (==2) list
                    ,length $ filter (==3) list
                    ,length $ filter (==4) list
                    ,length $ filter (==5) list
                    ,length $ filter (==6) list
                    ,length $ filter (==7) list
                    ,length $ filter (==8) list
                    ,length $ filter (==9) list]

getStarOrSpace :: Int -> Char
getStarOrSpace 0 = ' '
getStarOrSpace _ = '*'

decrementByOneIfInputIsGreaterThanZero :: Int -> Int
decrementByOneIfInputIsGreaterThanZero 0 = 0
decrementByOneIfInputIsGreaterThanZero x = x - 1


getHistogramWithCounts :: [Int] -> String
getHistogramWithCounts list = case list of
                                [0,0,0,0,0,0,0,0,0,0] -> ""
                                otherwise -> getHistogramWithCounts (map decrementByOneIfInputIsGreaterThanZero list) ++ map getStarOrSpace list ++ "\n"


histogram :: [Integer] -> String
histogram list = getHistogramWithCounts (getCountList list) ++ "==========\n0123456789\n"
