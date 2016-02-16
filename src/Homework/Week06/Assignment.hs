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
fib n
  | n == 0 = 0
  | n == 1 = 1
  | n >= 2 = fib (n - 1) + fib (n - 2)
  | otherwise = error "fib: input less than 0"

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- #2
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- #3
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream s ss) = s : streamToList ss

instance Show a => Show (Stream a) where
  show ss = concatMap (++ " ") vals ++ "..."
    where
      vals = map show (take 20 $ streamToList ss)

-- #4
streamRepeat :: a -> Stream a
streamRepeat s = Stream s (streamRepeat s)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream s ss) = Stream (f s) (streamMap f ss)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))

-- #5
nats :: Stream Integer
nats = streamFromSeed succ 0

ruler :: Stream Integer
ruler = go (streamMap streamRepeat nats)
  where
    go (Stream s ss) = interleaveStreams s (go ss)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream s ss) z = Stream s (interleaveStreams z ss)
