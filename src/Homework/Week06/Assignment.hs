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
fib x = fib (x-2) + fib (x-1)

fibs1 :: [Integer]
fibs1 = [ fib x | x <- [0..]]

-- #2
fibs2 :: [Integer]
fibs2 = [ fib x | x <- [0..]] -- come back to it

-- #3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (x `Cons` xs) = x : (streamToList xs)

instance Show a => Show (Stream a) where
  show st = (show (take 20 (streamToList st)))

-- #4
streamRepeat :: a -> Stream a
streamRepeat x = x `Cons` streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (x `Cons` xs) =  (f x) `Cons` (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = x `Cons` (streamFromSeed f (f x))

-- #5
nats :: Stream Integer
nats =  streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (x `Cons` xs) y = x `Cons` (interleaveStreams y xs)

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)
