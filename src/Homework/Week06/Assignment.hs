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


import Data.List

-- #1a
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibMemory :: Integer -> [Integer] -> Integer
fibMemory 0 _ = 0
fibMemory 1 _ = 1
fibMemory n mem = head mem

fibs1 :: [Integer]
fibs1 = fmap fib [0 .. ]

fibInf :: [Integer] -> [Integer]
fibInf (x : y : z) = x : fibInf [y , x + y]

-- #2
fibs2 :: [Integer]
fibs2 = fibInf [0,1]

-- #3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons item tailStream) = item : streamToList tailStream

instance Show a => Show (Stream a) where
  show (Cons item tailStream) = show item

-- #4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fun (Cons item streamTail) = Cons (fun item) (streamMap fun streamTail)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed fun input = Cons input (streamFromSeed fun (fun input))

-- #5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x firstTail) ~(Cons y secondTail) = Cons x (Cons y (interleaveStreams firstTail secondTail))

rulerStartFromZero :: Integer -> Stream Integer
rulerStartFromZero n = interleaveStreams (streamRepeat n) (rulerStartFromZero (n+1))

ruler :: Stream Integer
ruler = rulerStartFromZero 0
