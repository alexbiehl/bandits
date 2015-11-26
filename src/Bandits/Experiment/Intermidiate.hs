{-# LANGUAGE RecordWildCards #-}
module Bandits.Experiment.Intermidiate where

import           Bandits.Experiment.Types

import           Crypto.Hash              (Digest, SHA1)
import qualified Crypto.Hash              as Cryptonite
import qualified Data.ByteArray           as ByteArray
import qualified Data.ByteArray.Encoding  as ByteArray
import qualified Data.Text.Encoding       as Text

-- | Determistically create an ExperimentId from a Experiment.
--   It hashes the experiment name and takes the first 8 hex encoded
--   bytes.
mkExperimentId :: Experiment -> ExperimentId
mkExperimentId Experiment{..} = MkExperimentId (Text.decodeUtf8 hex)
  where
    hash = Cryptonite.hash (Text.encodeUtf8 expName) :: Digest SHA1
    hex = ByteArray.take 8 (ByteArray.convertToBase ByteArray.Base16 hash)
