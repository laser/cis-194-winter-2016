module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram,
  skips'
) where

import Data.List
import qualified Data.Map as Map
import Data.Maybe
-- #1
skips :: [a] -> [[a]]
skips xss = takeWhile (\x -> case x of
                        [] -> False
                        _  -> True ) $ map (doSkip xss) [1..]
            where doSkip xs n = map fst $ filter (\ (e,idx) -> idx `mod` n == 0 ) $ zip xs [1..]


skips' :: [a] -> [[a]]
skips' xs  = map (\ x-> case length x of
                           2 -> [head x]
                           3 -> [head x, last x]
                           _ -> x ) $ go xs
go :: [a] -> [[a]]
go [] = []
go xs = xs : go (tail xs)

-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs)
  | a < b && b > c = b : localMaxima (c : xs)
  | otherwise      = localMaxima (b : c : xs)
localMaxima _      = []

scale :: Int
scale = 10

indices :: [Int]
indices   = [0..(scale - 1)]

histogram :: [Int] -> String
histogram xs = makeLines ++ legend
               where makeLines  = loop "" $ theMap xs
                     theMap     = Map.fromList . map (\x -> (head x, length x) ) . group . sort
                     legend     = replicate scale '=' ++ "\n" ++ concatMap show indices ++ "\n"

loop :: String -> Map.Map Int Int -> String
loop theLines theMap = if (length theMap) == 0 then  theLines else loop (line ++ "\n" ++ theLines) restMxs
               where line     = map (starOrSpace . (`Map.lookup` theMap)) indices
                     restMxs  =  Map.filter(>0) $ Map.map pred theMap
                     starOrSpace x = if isJust x then '*' else ' '
