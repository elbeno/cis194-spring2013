{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Control.Applicative
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
                 deriving Show

-- exercise 2

-- the number of units that may be used to attack/defend

attackingUnits :: Battlefield -> Int
attackingUnits b = min 3 (attackers b - 1)

defendingUnits :: Battlefield -> Int
defendingUnits b = min 2 (defenders b)

-- roll some dice n times, sort the output (descending)

rollDice :: Int -> Rand StdGen [DieValue]
rollDice n = sortBy (flip compare) <$> replicateM n die

-- conduct a battle

battle :: Battlefield -> Rand StdGen Battlefield
battle b@(Battlefield a d) = do
  -- the number of units each side can use
  let (atk, def) = (attackingUnits b, defendingUnits b)
  -- each side's dice rolls
  atkRolls <- rollDice atk
  defRolls <- rollDice def
  -- how many losses for each?
  let outcomes = zipWith (-) atkRolls defRolls
  let atkLosses = length $ filter (<=0) outcomes
  let defLosses = length outcomes - atkLosses
  -- attackers lose some, defenders lose some
  return $ Battlefield (a-atkLosses) (d-defLosses)

-- exercise 3

-- is an invasion over?

invasionIsOver :: Battlefield -> Bool
invasionIsOver (Battlefield a d) = d == 0 || a < 2

-- invade (attack until success or failure)

invade :: Battlefield -> Rand StdGen Battlefield
invade b
  | invasionIsOver b = return b
  | otherwise = battle b >>= invade

-- exercise 4

-- did the attacker win an invasion?

attackerWon :: Battlefield -> Bool
attackerWon (Battlefield _ d) = d == 0

-- probability of success through Monte Carlo methods

successProb :: Battlefield -> Rand StdGen Double
successProb b = do
  invs <- replicateM 1000 (invade b)
  let wins = length $ filter attackerWon invs
  return $ fromIntegral wins / 1000.0
