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
fib n = fib(n-2) + fib(n-1)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- #2
fibs2 :: [Integer]
fibs2 = 0: 1: calcFib 0 1

calcFib :: Integer -> Integer -> [Integer]
calcFib n1 n2 = n1+n2 : (calcFib n2 (n1+n2))
-- #3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs)= x: streamToList xs

-- instance Show a => Show (Stream a) where
--   show =

-- #4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap = undefined

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed = undefined

-- #5
nats :: Stream Integer
nats = undefined

ruler :: Stream Integer
ruler = undefined