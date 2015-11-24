{-# LANGUAGE FlexibleContexts #-}
module Bandits.Experiment where

import           Bandits.Experiment.Instructions
import           Bandits.Experiment.Types

-- | Assigns a variation to a user belonging to an experiment.
assign :: RunExperiment m => ExperimentId -> UserId -> m Variation
assign expId uId = do
  mv <- lookupAssignment expId uId
  case mv of
    Just var -> return var
    Nothing -> newAssignment expId uId

-- | Tracks the success goal of a user belongig to an experiment.
reward :: RunExperiment m => ExperimentId -> UserId -> Reward -> m ()
reward expId uId rew = do
  mv <- lookupAssignment expId uId
  case mv of
    Just _ -> newReward expId uId rew
    Nothing -> return ()
