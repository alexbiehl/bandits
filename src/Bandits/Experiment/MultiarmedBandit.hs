{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
module Bandits.Experiment.MultiarmedBandit where

import           Bandits.Experiment.Instructions
import           Bandits.Experiment.Types

import           Data.Foldable
import qualified Data.Vector                     as Vector

-- | Creates a bandit from a bandit type.
mkBandit :: BanditType -> Bandit
mkBandit (BtEpsilonGreedy eps) = mkEpsilonGreedy eps
mkBandit (BtSoftmax tmp) = mkSoftmax tmp

-- | Epsilongreedy bandit.
mkEpsilonGreedy :: RunBandit m => Epsilon -> m Arm
mkEpsilonGreedy eps = do
  p <- randomProbability
  if p <= (1 - eps)
    then do Just (arm, _) <- scan best Nothing
            return arm
    else randomArm
  where
    best arm weight r@(Just (_, weight'))
      | weight >= weight' = Just (arm, weight)
      | otherwise = r
    best arm weight Nothing = Just (arm, weight)

mkSoftmax :: RunBandit m => Temprature -> m Arm
mkSoftmax t = do
  -- Normalize the weights respecting the temprature
  norms <- collect (\_ w -> exp (w / (t * 100)))
  p <- randomProbability
  let scale = foldl' (+) 0 norms
      -- scale the norms using the scale
      probs = fmap (/ scale) norms
      -- accumulate the probabilities
      acc = Vector.scanl (+) 0.0 probs
      -- find the index where the value is bigger than p.
      Just i = Vector.findIndex (>= p) acc
  chooseArm i
