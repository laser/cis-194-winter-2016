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

----------
-- Ex 1 --
----------

-- Translate the above definition of Fibonacci numbers directly into a
-- recursive function definition of type:
--
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
--
-- so that fib n computes the nth Fibonacci number Fn. Then, use fib to
-- define the infinite list of all Fibonacci numbers:
--
-- fibs1 :: [Integer]

fibs1 :: [Integer]
fibs1 = map fib [0..]

----------
-- Ex 2 --
----------

-- Define the infinite list:
--
-- fibs2 :: [Integer]
--
-- so that it has the same elements as fibs1, but computing the first n
-- elements of fibs2 requires only O(n) additional operations. Be sure to
-- use standard recursion pattern(s) from Prelude, as appropriate.
--

fibs2 :: [Integer]
fibs2 = map head $ iterate (\(a:b:c:ds) -> [b, c, (b+c)]) [0, 1, 1]

----------
-- Ex 3 --
----------

-- * Define a data type of polymorphic streams, Stream.

data Stream a = Cons a (Stream a)

-- * Write a function to convert a Stream to an infinite list:
--
--   streamToList :: Stream a -> [a]

streamToList :: Stream a -> [a]
streamToList (Cons x c) = x : streamToList c

--
-- * Make your own instance of Show for Stream:
--
instance Show a => Show (Stream a) where
  show s = show $ take 20 $ streamToList s
--
--   ...which works by showing only some prefix of a stream (say, the first
--   20 elements)

----------
-- Ex 4 --
----------

-- * Write a function:
--
-- ...which generates a stream containing infinitely many copies of the
-- given element
streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

-- * Write a function:
--
-- ...which applies a function to every element of a Stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) $ streamMap f s

-- * Write a function:
--
-- ...which generates a Stream from a “seed” of type a, which is the first
-- element of the stream, and an “unfolding rule” of type a -> a which
-- specifies how to transform the seed into a new seed, to be used for
-- generating the rest of the stream.
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f (f x)

----------
-- Ex 5 --
----------

-- * Define the stream:
--
nats :: Stream Integer
nats = streamFromSeed (+1) (0 :: Integer)
--
-- ...which contains the infinite list of natural numbers 0, 1, 2...
--
-- * Define the stream:
--
-- ruler :: Stream Integer
--
-- ...which corresponds to the ruler function:
--
-- 0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,...
--
-- ...where the nth element in the stream (assuming the first element
-- corresponds to n = 1) is the largest power of 2 which evenly divides n.
--
-- Hint: define a function interleaveStreams which alternates the
-- elements from two streams. Can you use this function to implement ruler
-- in a clever way that does not have to do any divisibility testing?

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x1 s1) (Cons x2 s2) = Cons x1 $ Cons x2 $ interleaveStreams s1 s2

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap highestPowOfTwoThatDivsEvenly $ streamFromSeed (+2) 2)

highestPowOfTwoThatDivsEvenly :: Integer -> Integer
highestPowOfTwoThatDivsEvenly n = go 1
  where
    go acc = if isInt (quotient (acc + 1)) then go (acc + 1) else acc
    isInt x = x == fromInteger (round x)
    quotient pow = (fromIntegral n) / 2**(fromIntegral pow)