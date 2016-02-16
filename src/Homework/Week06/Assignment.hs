module Homework.Week06.Assignment (
  fib,
  fibs1,
  fibs2,
  streamToList,
  streamRepeat,
  streamMap,
  streamFromSeed,
  ruler,
  nats,
  Stream(..)
) where

import           Data.List

-- #1a
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- #2
fibs2 :: [Integer]
fibs2 = 0:1:zipWith (+) fibs2 (tail fibs2)

-- #3
data Stream a = Conz a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Conz x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show s = show $ take 10 $ streamToList s

-- #4
streamRepeat :: a -> Stream a
streamRepeat x = Conz x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn (Conz x xs) = Conz (fn x) $ streamMap fn xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed fn x = Conz x $ streamFromSeed fn $ fn x

-- #5
nats :: Stream Int
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams ~(Conz x xs) ~(Conz y ys) = Conz x $ Conz y $ interleaveStreams xs ys
--tildes force lazy pattern matching. Without them, ruler never terminates

rulerBuilder :: Int -> Stream Int
rulerBuilder n = interleaveStreams (streamRepeat n) $ rulerBuilder (n+1)

ruler :: Stream Int
ruler = rulerBuilder 0

-- how this works:
-- ruler = interleaveStreams zeros as
--   where zeros = streamRepeat 0
--         as    = interleaveStreams ones bs
--         ones  = streamRepeat 1
--         bs    = interleaveStreams twos cs
--         twos  = streamRepeat 2
--         cs    = interleaveStreams (streamRepeat 3) (streamRepeat 4)
