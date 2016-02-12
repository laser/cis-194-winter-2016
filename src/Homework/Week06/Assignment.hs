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
  , rubegoldfibz
  , poorslidinfibz
) where

--import Data.Map

-- #1a
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n =  (fib $ n - 1)  + (fib $ n - 2)

fibs1 :: [Integer]
fibs1 = fmap fib [0..]
--
-- try this one with iterate
-- ooooo

-- #2
fibs2 :: [Integer]
fibs2 = poorslidinfibz

-- to be O(n) we have to retain previous calculations
-- so recursion passing a map or container is what we need to do
-- and we cant compute things going off in two par recursions because we have to share the same map/container
-- goFib ( n , [(0,0),(1,1)] ).....
-- goFib n priors =  lookup n priors
--                   if found in priors then done
--                   otherwise compute fib for n-2 and then n-1  storing both in priors container


-- poppin fibz the hard way... oh yeah!
rubegoldfibz :: Int -> [Integer]
rubegoldfibz n = fmap snd $ reverse $ last $ take n $ iterate fibberator [(0,0)]
                 where fibberator xs@((0,0) :[])           = (1,1) : xs
                       fibberator xs@((a,b):(c,d) :_)      = (succ a, b + d ) : xs

-- popz teh fibz with a poor man's slider
poorslidinfibz :: [Integer]
poorslidinfibz = fmap snd $ go [(0,0),(1,1)]
                where go all@(x : xs) = x : ( go $ last xs :  nextFibs last2 )
                                        where last2 = take 2 $ drop (length all - 2)  all
                                              nextFibs [(a,b),(c,d)]= [(succ c, b + d)]

-- given n and map
-- lookup n in map
-- if found use n
-- else ....??? what now???
--   store n -> n-1 + n -2
--

--
-- #3
--
data Stream a = Stream a -- replace this with your own definition; this one is wrong

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
