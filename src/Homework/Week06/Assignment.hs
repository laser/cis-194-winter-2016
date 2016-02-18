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
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- #2
fibs2 :: [Integer]
fibs2 = undefined

-- #3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a ss) = a : streamToList ss

instance Show a => Show (Stream a) where
    show = show . (take 20) . streamToList

-- #4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x ss) = Cons (f x) (streamMap f ss)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- #5
nats :: Stream Integer
nats = streamFromSeed ( + 1) 0

ruler :: Stream Integer
ruler = foldr1 interleaveStreams (map streamRepeat [0..])

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)
