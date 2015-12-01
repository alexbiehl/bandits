{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Bandits.Experiment where

import           Bandits.Experiment.Instructions
import           Bandits.Experiment.Types

import           Crypto.Hash                     (Digest, SHA256)
import qualified Crypto.Hash                     as Cryptonite
import qualified Data.ByteArray                  as ByteArray
import qualified Data.ByteArray.Encoding         as ByteArray
import qualified Data.Text.Encoding              as Text

-- | Determistically create an ExperimentId from a Experiment.
--   It hashes the experiment name and takes the first 12 hex encoded
--   bytes.
mkExperimentId :: Experiment -> ExperimentId
mkExperimentId Experiment{..} = MkExperimentId (Text.decodeUtf8 hex)
  where
    hash = Cryptonite.hash (Text.encodeUtf8 expName) :: Digest SHA256
    hex = ByteArray.take 12 (ByteArray.convertToBase ByteArray.Base16 hash)

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
