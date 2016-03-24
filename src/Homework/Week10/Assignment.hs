{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Homework.Week10.Assignment (
  DieValue(..),
  die
)where

import           Control.Monad
import           Control.Monad.Random

import           Data.List

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

-- #2 (there is no assignment #1, really)

-- Risk for one round of one fight. Should do three things:
-- 1) roll appropriate # of dice
--  - attacker may attack w/ up to 3 units at a time, but must leave 1 behind
--  - defender may defend w/ up to 2 units
--  - assume max possible for each side
--  - one die for each unit
--    - so: attacker rolls 1/2/3 dice, defender rolls 1/2
-- 2) interpret results
--  - sort dice rolls descending, match them up in pairs
--  - defender wins ties
-- 3) update armies to reflect casualties

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

getAttackers :: Battlefield -> Army
getAttackers bf@(att, _)
  | att <= 1  = 0
  | att >= 4  = 3
  | otherwise = att

getDefenders :: Battlefield -> Army
getDefenders bf@(_, def)
  | def > 2 = 2
  | otherwise = def

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
  attackRolls <- sort <$> dice att
  defenseRolls <- sort <$> dice def
  return bf
  where att = getAttackers bf
        def = getDefenders bf

-- #3
invade :: Battlefield -> Rand StdGen Battlefield
invade = undefined

-- #4
successProb :: Battlefield -> Rand StdGen Double
successProb = undefined

-- #5
exactSuccessProb :: Battlefield -> Double
exactSuccessProb = undefined
