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
  interleaveStreams,
  Stream(..)
) where

-- #1a
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


fibs1 :: [Integer]
fibs1 = map  fib [0..]

-- #2
fibs2 :: [Integer]
fibs2 = fibo 0 1
    where fibo x y = x : (fibo y (x + y))
    

-- #3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a (s)) = a : streamToList s

instance Show a => Show (Stream a) where 
  show = show . (take 20) . streamToList

-- #4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a (s)) = Cons (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

-- #5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a0 (s0))(Cons a1 (s1)) = Cons a0 (Cons a1 (interleaveStreams s0 s1))

ruler :: Stream Integer

-- the divisibility way...
ruler = interleaveStreams (streamRepeat 0) (streamMap highestPowerOf2ThatEvenlyDivides (streamFromSeed (+2) 2))
    where highestPowerOf2ThatEvenlyDivides n = if mod n 2 == 0 
                                               then 1 + highestPowerOf2ThatEvenlyDivides (div n 2) 
                                               else 0

-- the clever way, but how to create a recursive function that will evaluate lazily?
-- we need something like this:
--
--ruler = interleaveStreams (streamRepeat 0)
--            (interleaveStreams (streamRepeat 1)
--                (interleaveStreams (streamRepeat 2)
--                    (interleaveStreams (streamRepeat 3)
--                        (interleaveStreams (streamRepeat 4)(streamRepeat 5)))))
--
--  ... but this never terminates...  :-(
--
--ruler = nestedInterleave 0
--    where nestedInterleave n = interleaveStreams (streamRepeat n) (nestedInterleave $ (n + 1))

