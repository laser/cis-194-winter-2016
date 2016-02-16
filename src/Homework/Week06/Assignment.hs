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
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- #2
fibs2 :: [Integer]
fibs2 = [0,1] ++ fibs2' 0 1 [2..] where
  fibs2' n1 n2 (_:xs) = [(n1 + n2)] ++ fibs2' n2 (n1 + n2) xs

-- #3
--data Stream a = Stream a -- replace this with your own definition; this one is wrong
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : streamToList s

instance Show a => Show (Stream a) where
  show s = show $ take 20 $ streamToList s

-- #4
streamRepeat :: a -> Stream a
streamRepeat s = Cons s (streamRepeat s)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- #5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x $ interleaveStreams ys xs

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)
