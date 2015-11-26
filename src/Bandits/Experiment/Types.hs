{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Bandits.Experiment.Types where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Hashable
import           Data.Text           (Text)
import           Data.Time
import           Data.Vector         (Vector)
import           GHC.Generics
import           Servant.Common.Text

-- | Represents a proband to an experiment.
newtype UserId = MkUserId Text
                 deriving ( Eq, Ord, Show, ToJSON, FromJSON, NFData, ToText
                          , FromText, Hashable )

-- | A reference to an (hopefully) existing experiment.
newtype ExperimentId = MkExperimentId Text
                       deriving ( Eq, Ord, Show, ToJSON, FromJSON, NFData
                                , ToText, FromText, Hashable )

-- | Represents a choosing from the underlying experiment.
newtype Variation = MkVariation Text
                    deriving ( Eq, Show, ToJSON, FromJSON, NFData )

-- | An experiment design is a collection of variations from which to
--   choose one for each proband.
newtype Design = MkDesign (Vector Variation)
                 deriving ( Eq, Show, ToJSON, FromJSON, NFData )

-- | A reward for a variation.
newtype Reward = MkReward Double
                 deriving ( Eq, Ord, Show, ToJSON, FromJSON, NFData )

-- | Collects all information about an experiment.
data Experiment =
  Experiment { expName   :: Text
             , expDescr  :: Text
             , expStart  :: Day
             , expEnd    :: Day
             , expDesign :: Design
             } deriving ( Eq, Show, Generic )

instance FromJSON Experiment where
  parseJSON v =
    Experiment <$> parseJSON v
               <*> parseJSON v
               <*> undefined
               <*> undefined
               <*> parseJSON v
