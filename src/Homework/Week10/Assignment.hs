{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Homework.Week10.Assignment (
  Battlefield(..),
  battle,
  invade,
  successProb
) where

import Control.Monad
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
  deriving Show

-- #2 (there is no assignment #1, really)
dice :: Int -> Rand StdGen [DieValue]
dice = flip replicateM die

clamp :: Ord r => r -> r -> r -> r
clamp low n high = min high $ max n low

accumulateRoll :: Ord a => Battlefield -> (a, a) -> Battlefield
accumulateRoll field (attack, defense)
  | attack > defense = field { attackers = attackers field - 1 }
  | otherwise        = field { defenders = defenders field - 1 }

battle :: Battlefield -> Rand StdGen Battlefield
battle field@(Battlefield attack defense) = do
  let effectiveAttackers = clamp 0 (attack - 1) 3
  let effectiveDefenders = clamp 0 defense      2

  attackRolls  <- sort <$> dice effectiveAttackers
  defenseRolls <- sort <$> dice effectiveDefenders

  let rollPairs = zip attackRolls defenseRolls
  return $ foldl' accumulateRoll field rollPairs

-- #3
invade :: Battlefield -> Rand StdGen Battlefield
invade field@(Battlefield attack defense)
  | defense == 0 = return field
  | attack  <= 1 = return field
  | otherwise    = invade =<< battle field

-- #4
isSuccess :: Battlefield -> Bool
isSuccess = (== 0) . defenders

successProb :: Battlefield -> Rand StdGen Double
successProb = fmap ((/ 1000) . genericLength . filter isSuccess) . replicateM 1000 . invade

-- #5
exactSuccessProb :: Battlefield -> Double
exactSuccessProb = undefined
