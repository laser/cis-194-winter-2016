module Homework.Week06.Assignment (
  fib,
  fibs1,
  fibs2,
  streamToList,
  streamRepeat,
  streamMap,
  streamFromSeed,
  nats,
  ruler,
  Stream(..)
) where

-- #1a
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

-- long way
fibs1 :: [Integer]
fibs1 = map fib [0..]

-- fibsh :: [Integer] -> [Integer]
-- fibsh [] = [0]
-- fibsh [0] = 1 : [0]
-- fibsh [1, 0] = 1 : 1 : [0]
-- fibsh zs@((x : y : z)) = (x + y) : zs

-- #2
fibs2 :: [Integer]
fibs2  = let xs   = fibs2
             xs'  = drop 1 $ xs -- 1 1 2 3 5
         in 0:1: zipWith (+) xs xs'

-- #3
data Stream a =  Stream a (Stream a) -- l

streamToList :: Stream a -> [a]
streamToList (Stream a as) = a : streamToList as

-- instance Show a => Show (Stream a) where
--   show xs  = take 20 $ map show streamToList

-- #4
streamRepeat :: a -> Stream a
--streamRepeat x = Stream x $ Stream x
streamRepeat x = xs
   where xs = Stream x xs

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f $ f x)

-- #5
nats :: Stream Integer
nats = streamFromSeed (+1) (0 :: Integer)

--list version:
-- interleave :: [a] -> [a] -> [a]
-- interleave (x : xs) ys = x :interleave ys xs

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x  xs) (Stream y ys) =
   Stream x (Stream y (interleaveStreams xs ys))

-- divTot :: Integer -> Integer -> Integer
-- divTot n cnt
--     | n `mod` 2 == 0   = divTot (n `div` 2) (cnt + 1)
--     | otherwise        = cnt
--
-- flipDiv :: Integer -> Integer
-- flipDiv = flip divTot 0
--
-- divAll :: Stream Integer -> Stream Integer
-- divAll = undefined --fmap (flip divTot 0)


ruler :: Stream Integer
ruler = undefined --streamFromSeed (flipDiv) (0::Integer)
