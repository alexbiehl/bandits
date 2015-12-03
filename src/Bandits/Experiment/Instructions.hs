{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
module Bandits.Experiment.Instructions where

import           Bandits.Experiment.Types

import           Control.Monad.Free
import           Data.Foldable
import           Data.Functor.Sum
import           Data.Vector              (Vector)
import qualified Data.Vector              as Vector

-- | Effects for running experiment actions.
type RunExperiment m = ( MonadFree (Sum ExpInstr BanditInstr) m )

-- | The basic operations for running an experiment are encoded as
--   free monad over the functor of `ExpInstr`.
data ExpInstr a
     = LookupAssignment ExperimentId UserId (Maybe Variation -> a)
       -- ^ Looks up an assignment for a user belongig to a specific experiment.
     | NewAssignment ExperimentId UserId (Variation -> a)
       -- ^ Assigns a variation to user belonging to a specific experiment.
     | NewReward ExperimentId UserId Reward a
       -- ^ Tracks the goal of a user belongig to an experiment.
     deriving ( Functor )

-- | Looks up an assignment for a user belongig to a specific experiment.
lookupAssignment :: RunExperiment m => ExperimentId -> UserId -> m (Maybe Variation)
lookupAssignment expId uId = liftF (InL (LookupAssignment expId uId id))

-- | Assigns a variation to user belonging to a specific experiment.
newAssignment :: RunExperiment m => ExperimentId -> UserId -> m Variation
newAssignment expId uId = liftF (InL (NewAssignment expId uId id))

-- | Tracks the goal of a user belongig to an experiment.
newReward :: RunExperiment m => ExperimentId -> UserId -> Reward -> m ()
newReward expId uId rew = liftF (InL (NewReward expId uId rew ()))

-- | A lightweight base monad for bandits.
type RunBandit m = RunExperiment m

type Bandit = forall m. RunBandit m => m Arm

type Arm = Variation

type Weight = Float

-- | Some backend agnostic instructions every bandit understands.
data BanditInstr a = RandomProbability (Float -> a)
                   | RandomArm (Arm -> a)
                   | ChooseArm Int (Arm -> a)
                   | forall x. Scan (Arm -> Weight -> x -> x) x (x -> a)
                   | forall x. Collect (Arm -> Weight -> x) (Vector x -> a)

deriving instance Functor BanditInstr

-- | Generate a random probability (0 <= p <= 1.0).
randomProbability :: RunBandit m => m Float
randomProbability = liftF (InR (RandomProbability id))

-- | Well, bandit doesn't care what arm is taken. Choose it randomly.
randomArm :: RunBandit m => m Arm
randomArm = liftF (InR (RandomArm id))

-- | Choose an arm with an index
chooseArm :: RunBandit m => Int -> m Arm
chooseArm n = liftF (InR (ChooseArm n id))

-- | Sometimes bandit needs to examine the arms and their weights.
scan :: RunBandit m => (Arm -> Float -> a -> a) -> a -> m a
scan f n = liftF (InR (Scan f n id))

-- | Collect the arms and their weights in a transformation.
collect :: RunBandit m => (Arm -> Float -> a) -> m (Vector a)
collect f = liftF (InR (Collect f id))
