module Homework.Week6.Assignment where

-- #1a
fib :: Integer -> Integer
fib = undefined

fibs1 :: [Integer]
fibs1 = undefined

-- #2
fibs2 :: [Integer]
fibs2 = undefined

-- #3
data Stream a = Stream a

streamToList :: Stream a -> [a]
streamToList = undefined

-- instance Show a => Show (Stream a) where
--   show = ???

-- #4
streamRepeat :: a -> Stream a
streamRepeat = undefined

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap = undefined

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed = undefined

-- #5
nats :: Stream Integer
nats = undefined

ruler :: Stream Integer
ruler = undefined