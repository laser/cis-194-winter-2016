{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Homework.Week10.Assignment where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1, 6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show, Eq)

-- #2 (there is no assignment #1, really)
battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
  attackRolls <- rolls (attackers bf)
  defendRolls <- rolls (defenders bf)
  return $ foldr (uncurry fatal) bf (zip attackRolls defendRolls)

fatal :: DieValue -> DieValue -> Battlefield -> Battlefield
fatal aRoll dRoll bf
  | aRoll > dRoll = bf { defenders = defenders bf - 1 }
  | otherwise = bf { attackers = attackers bf - 1 }

rolls :: Int -> Rand StdGen [DieValue]
rolls n = (reverse . sort) <$> sequence (replicate n die)

-- #3
invade :: Battlefield -> Rand StdGen Battlefield
invade bf = do
  bf' <- battle bf
  (if attackers bf' == 0 || defenders bf' == 0 then return else invade) bf'

-- #4
successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  bfs <- sequence $ replicate 1000 (invade bf)
  let success = sum $ map (\b -> if 0 == defenders b then 1 else 0) bfs
  return $ success / 1000

-- #5
exactSuccessProb :: Battlefield -> Double
exactSuccessProb (Battlefield a d) = aWeight / (aWeight + dWeight) -- Normalize
  where
    -- Weights: Given a die roll for each [1..6] what is the chance of winning against another die roll.
    aWeight = sum [x ^ a / 6 | x <- [0..5]]
    dWeight = sum [x ^ d / 6 | x <- [1..6]]
