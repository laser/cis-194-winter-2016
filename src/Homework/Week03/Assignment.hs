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
localMaxima1 :: [Integer] -> [Integer]
localMaxima1 [] = []
localMaxima1 (x1:[]) = []
localMaxima1 (x1:x2:[]) = []
localMaxima1 (x1:x2:x3:xs) = case x1 < x2 && x2 > x3 of
                              True -> x2 : localMaxima1(x2:x3:xs)
                              False -> localMaxima1(x2:x3:xs)

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (\(_:x2:_) -> x2) (filter (\x -> case x of
                                                           x1:[] -> False
                                                           x1:x2:[] -> False
                                                           x1:x2:x3:[] -> (x1 < x2 && x2 > x3)) (group3 xs))
--localMaxima xs = filter (\_ -> True ) (group3 xs)

group3 :: [a] -> [[a]]
group3 xs = map snd (map unzip (map (\(skipPosition,_) -> filter(\(p, e) -> (skipPosition-p == -1) || (skipPosition-p == 0) || (skipPosition-p == 1)) zipWithPosition) zipWithPosition))
    where zipWithPosition = zip [1..] xs

-- #3
histogram :: [Integer] -> String
histogram = undefined
