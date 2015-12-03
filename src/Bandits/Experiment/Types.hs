{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Bandits.Experiment.Types where

import           Control.DeepSeq
import           Control.Monad
import           Data.Aeson
import           Data.Hashable
import           Data.Text           (Text)
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
newtype Alternatives = MkAlternatives (Vector Variation)
                     deriving ( Eq, Show, ToJSON, FromJSON, NFData )

-- | A reward for a variation.
newtype Reward = MkReward Double
                 deriving ( Eq, Ord, Show, ToJSON, FromJSON, NFData )

-- | Represents the probability with which the epsilongreedy bandit
--   explores new alternatives. (0 <= eps <= 1.0)
type Epsilon = Float

-- | The temrature for the softmax bandit.
type Temprature = Float

-- | The available bandit types with their respective parameters.
data BanditType = BtEpsilonGreedy Epsilon
                  -- ^ Chooses the epsilongreedy bandit.
                | BtSoftmax Temprature
                  -- ^ Chooses the softmax bandit.
                deriving (Eq, Read, Show)

-- | Collects all information about an experiment.
data Experiment =
  Experiment { expName         :: Text
               -- ^ A name for the experiment.
             , expDescr        :: Text
               -- ^ A description for an experiment.
             , expAlternatives :: Alternatives
               -- ^ The variations for the experiment.
             , expBanditType   :: BanditType
               -- ^ The specific bandit to use.
             } deriving ( Eq, Show, Generic )

instance FromJSON BanditType where
  parseJSON = withObject "bandit" $ \o -> do
    bt <- o .: "type"
    case (bt :: Text) of
      "epsilon_greedy" ->
        BtEpsilonGreedy <$> o .:? "epsilon" .!= 0.1
      "softmax" ->
        BtSoftmax <$> o .: "temperature" .!= 0.2
      _ -> mzero

instance FromJSON Experiment where
  parseJSON = withObject "experiment" $ \o ->
    Experiment <$> o .: "name"
               <*> o .: "description"
               <*> o .: "alternatives"
               <*> o .: "bandit"
