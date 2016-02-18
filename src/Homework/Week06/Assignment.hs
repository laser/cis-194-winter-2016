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

import Debug.Trace

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

instance Show a => Show (Stream a) where
   show = show . take 10 . streamToList

-- #4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- #5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap f (streamFromSeed (+1) 1)
  where
      f x = if((x `mod` 2) /= 0) then 0 else (by2 x 1)

by2 :: Integer -> Integer -> Integer
by2 x accu = --trace("x:" ++ show x ++ " accu:" ++ show accu ++ " result: " ++ show(x `mod` power2 accu))
             (if(x `mod` power2 accu /= 0) then (accu-1) else by2 x (accu+1))

power2 :: Integer -> Integer
power2 0 = 1
power2 x = 2 * power2 (x-1)