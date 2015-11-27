{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
module Bandits.Experiment.MultiarmedBandit where

import           Bandits.Experiment.Types

import           Control.Monad.Free
import           Data.Vector              (Vector)
import qualified Data.Vector as Vector

-- | An arm is a variation in our case.
type Arm = Variation

-- | Every arm has a weight which indicates it's successfulness.
type Weight = Float

-- | A lightweight base monad for bandits.
type RunBandit m = ( MonadFree BanditInstr m )

type Bandit = forall m. RunBandit m => m Arm

-- | Some backend agnostic instructions every bandit understands.
data BanditInstr a = RandomProbability (Float -> a)
                   | RandomArm (Arm -> a)
                   | forall x. Scan (Arm -> Weight -> x -> x) x (x -> a)
                   | forall x. Collect (Arm -> Weight -> x) (Vector x -> a)

deriving instance Functor BanditInstr

-- | Creates a bandit from a bandit type.
mkBandit :: BanditType -> Bandit
mkBandit (BtEpsilonGreedy eps) = mkEpsilonGreedy eps
mkBandit (BtSoftmax tmp) = mkSoftmax tmp

-- | Generate a random probability (0 <= p <= 1.0).
randomProbability :: RunBandit m => m Float
randomProbability = liftF (RandomProbability id)

-- | Well, bandit doesn't care what arm is taken. Choose it randomly.
randomArm :: RunBandit m => m Arm
randomArm = liftF (RandomArm id)

-- | Sometimes bandit needs to examine the arms and their weights.
scan :: RunBandit m => (Arm -> Float -> a -> a) -> a -> m a
scan f n = liftF (Scan f n id)

-- | Collect the arms and their weights in a transformation.
collect :: RunBandit m => (Arm -> Float -> a) -> m (Vector a)
collect f = liftF (Collect f id)

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
mkSoftmax = undefined
