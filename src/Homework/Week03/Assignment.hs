module Homework.Week03.Assignment (
  skips,
  localMaxima,
  histogram
) where

import Data.List

-- #1
skips :: [a] -> [[a]]
skips xss = takeWhile (\x -> case x of
                        [] -> False
                        _  -> True ) $ map (doSkip xss) [1..]
            where doSkip xs n = map fst $ filter (\ (e,idx) -> idx `mod` n == 0 ) $ zip xs [1..]


-- #2
localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs)
  | a < b && b > c = b : localMaxima (c : xs)
  | otherwise      = localMaxima (b : c : xs)                     
localMaxima _      = []



-- #3
histogram :: [Integer] -> String
histogram xs  =  lines ++ tallyLine ++ legend ++ ['\n']
   where lines = foldr (++) "" $ reverse $ doRun $ group $ sort xs
         tallyLine  = (take 10 $ repeat '=') ++ ['\n']
         legend     = concat $ map show $ take 10 $ [0..]

emptyLine :: String
emptyLine = (take 10 $ repeat ' ') ++   ['\n']
          
doRun ::  [[Integer]] -> [String]
doRun [] = []
doRun xsxs =  currLine : (doRun next)
    where (curr,next) = scrapeTheTop xsxs
          currLine    = insertSequence emptyLine curr

scrapeTheTop :: Eq t => [[t]] -> ([t], [[t]])
scrapeTheTop xsxs = (concat curr, filter (/= []) next)
             where (curr,next) =  juxt (take 1) (drop 1) xsxs
                   
insertSequence :: Foldable t => String ->  t Integer -> String
insertSequence line = foldl insertInLine line

insertInLine :: String -> Integer -> String
insertInLine line idx = front ++ ['*'] ++ (drop 1 back)
  where (front,back) = splitAt (fromIntegral idx)  line

juxt :: (a -> b) -> (a -> b1) -> [a]-> ([b], [b1])           
juxt f1 f2 xs = (map f1 xs,map f2 xs)
