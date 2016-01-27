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
                                                            x1:x2:x3:[] -> (x1 < x2 && x2 > x3)
                                                            _ -> False
                                                           ) (group3 xs))

group3 :: [a] -> [[a]]
group3 xs = map snd (map unzip (map (\(skipPosition,_) -> filter(\(p, e) -> (skipPosition-p == -1) || (skipPosition-p == 0) || (skipPosition-p == 1)) zipWithPosition) zipWithPosition))
    where zipWithPosition = zip [1..] xs

-- #3
histogram :: [Integer] -> String
histogram = toString . printLines

toString :: [[String]] -> String
toString xs = unlines ((printLineString xs) ++ [(replicate 10 '=')] ++ [concat $ map show [0..9]])
    where   printLineString xs = cleanLineString ( map concat xs)
            cleanLineString xs = dropWhile (\x -> words x == []) xs

printLines :: [Integer] -> [[String]]
printLines xs = map(\row -> printRow row . countAll $ xs)[9,8..1]
    where   printRow row counts = map (\count -> if(count-row > -1) then "*" else " ") counts
            countAll xs         = map (\x -> countFor x xs)[0..9]
            countFor r xs       = length . filter (==r) $ xs



