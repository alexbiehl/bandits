{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bandits.Experiment.Instructions where

import           Bandits.Experiment.Types

import           Control.Monad.Free

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

-- | Effects for running experiment actions.
type RunExperiment m = ( MonadFree ExpInstr m )

-- | Looks up an assignment for a user belongig to a specific experiment.
lookupAssignment :: RunExperiment m => ExperimentId -> UserId -> m (Maybe Variation)
lookupAssignment expId uId = liftF (LookupAssignment expId uId id)

-- | Assigns a variation to user belonging to a specific experiment.
newAssignment :: RunExperiment m => ExperimentId -> UserId -> m Variation
newAssignment expId uId = liftF (NewAssignment expId uId id)

-- | Tracks the goal of a user belongig to an experiment.
newReward :: RunExperiment m => ExperimentId -> UserId -> Reward -> m ()
newReward expId uId rew = liftF (NewReward expId uId rew ())
