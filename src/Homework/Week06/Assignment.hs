{-# LANGUAGE FlexibleInstances #-}

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
  ruler2,
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

-- #2
fibs2 :: [Integer]
fibs2 = poorslidinfibz

-- poppin fibz the hard way... oh yeah!
rubegoldfibz :: Int -> [Integer]
rubegoldfibz n = fmap snd $ reverse $ last $ take n $ iterate fibberator [(0,0)]
                 where fibberator xs@((0,0) :[])           = (1,1) : xs
                       fibberator xs@((a,b):(c,d) :_)      = (succ a, b + d ) : xs

-- popz teh fibz with a poor man's slider
poorslidinfibz :: [Integer]
poorslidinfibz = fmap snd $ go [(0,0),(1,1)]
                where go all@(x : xs) = x : ( go $ last xs :  nextFibs lastTwo )
                                        where lastTwo = take 2 $ drop (length all - 2)  all
                                              nextFibs [(a,b),(c,d)]= [(succ c, b + d)]

--
-- #3
--
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

instance Show a => Show (Stream a) where
   show (Cons a s)= (show a) ++ "," ++ (show s)

-- #4
streamRepeat :: a -> Stream a
streamRepeat a = Cons a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) $ streamMap f s

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f e = Cons e $ streamFromSeed f (f e)

-- #5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap highestPowerOfTwo $ streamMap succ nats
        where highestPowerOfTwo n = toInteger  $ pred $ length $ takeWhile (==0) $ streamToList  $ streamMap (mod n) $ streamMap (2^) nats

-- #5a interleave....
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) (Cons b bs) = Cons a $ Cons b $ interleaveStreams as bs

-- for all odd numbers, for any power of 2 the gcd will be 1
--   so why bother computing them?
--   just get the gcd between 2 and any of the evens

ruler2 :: Stream Integer
ruler2 = interleaveStreams streamRepresentingOdds streamRepresentingEvens
        where streamRepresentingOdds     = streamRepeat 0
              streamRepresentingEvens    = streamFromSeed (+2) 2

-- use gcd to get highest power of two :)

-- take #1  bah! didnt need interleave


