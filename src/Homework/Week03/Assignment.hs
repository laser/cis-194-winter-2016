module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

-- #1
skips :: [a] -> [[a]]
skips xs = map snd (map unzip (map (\skipPosition -> filter(\(position, element) -> position `mod` skipPosition == 0) zipWithPosition) positions))
    where zipWithPosition = zip [1..] xs
          positions = fst (unzip zipWithPosition)

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima = localMaxima1

localMaxima1 :: [Integer] -> [Integer]
localMaxima1 (x1:x2:x3:xs) 
                | x1 < x2 && x2 > x3 =  x2 : localMaxima1(x2:x3:xs)
                | otherwise =  localMaxima1(x2:x3:xs)
localMaxima1 _ = []

localMaxima2 :: [Integer] -> [Integer]
localMaxima2 xs = map (\(_:x2:_) -> x2) (filter (\x -> case x of
                                                           x1:[] -> False
                                                           x1:x2:[] -> False
                                                           x1:x2:x3:[] -> (x1 < x2 && x2 > x3)) (group3 xs))
--localMaxima xs = filter (\_ -> True ) (group3 xs)

group3 :: [a] -> [[a]]
group3 xs = map snd (map unzip (map (\(skipPosition,_) -> filter(\(p, e) -> (skipPosition-p == -1) || (skipPosition-p == 0) || (skipPosition-p == 1)) zipWithPosition) zipWithPosition))
    where zipWithPosition = zip [1..] xs

-- #3
histogram :: [Integer] -> String
histogram xs = printStr . printLine $ xs

--filterNum :: Integer -> Integer -> Bool
--filterNum radix num = num == radix

countFor :: Integer -> [Integer] -> Int
countFor r xs = length . filter (==r) $ xs

count0to9 :: [Integer] -> [Int]
count0to9 xs = map (\x -> countFor x xs)[0..9]

printFor :: Int -> [Int] -> [String]
printFor pos xs = map (\count -> if(count-pos > -1) then "*" else " ") xs 

printLine :: [Integer] -> [[String]]
printLine xs = map(\x -> printFor x (count0to9 xs))[9,8..1]

printLineString :: [[String]] -> [String]
printLineString xs = cleanLineString ( map concat xs)

cleanLineString :: [String] -> [String]
cleanLineString xs = dropWhile (\x -> words x == []) xs 

printStr :: [[String]] -> String
printStr xs = unlines ((printLineString xs) ++ [(replicate 10 '=')] ++ [concat $ map show [0..9]])

