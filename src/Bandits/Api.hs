{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Bandits.Api where

import           Bandits.Experiment.Types

import           Control.Monad.Trans.Either
import           Data.Aeson                 hiding (Result)
import           Data.Proxy
import           Data.Text                  (Text)
import           Servant.API
import           Servant.Server

-- | Assigns a variation to a user belonging to an experiment.
type Assign = "experiments"
           :> Capture "experiment" ExperimentId
           :> "assign"
           :> Capture "user" UserId
           :> Get '[JSON] (Result Variation)

-- | Rewards the variation in an experiment for a user.
type Success = "experiments"
            :> Capture "experiment" ExperimentId
            :> "success"
            :> Capture "user" UserId
            :> Get '[JSON] (Result ())

-- | Creates and persists an experiment. Returns an experiment id to use
--   for assign and success.
type MkExperiment = "experiments"
                 :> "create"
                 :> ReqBody '[JSON] Experiment
                 :> Post '[JSON] (Result ExperimentId)

-- | A bundled api type for convenient server creation.
type ExperimentApi = Assign
                :<|> Success
                :<|> MkExperiment

type BanditsApi = ExperimentApi
             :<|> "static" :> Raw

banditsApi :: Proxy BanditsApi
banditsApi = Proxy

-- | A result from a route.
data Result a = ResOk a
                -- ^ Represents a good response
              | ResErr Int String Text
                -- ^ An error response contains a http status, status phrase and
                --   an error messge

instance ToJSON a => ToJSON (Result a) where
  toJSON (ResOk a) =
    object [ "result" .= toJSON a ]
  toJSON (ResErr status _ err) =
    object [ "error" .= status
           , "error_msg" .= err
           ]

resultToServantErr :: ToJSON a => Result a -> EitherT ServantErr IO (Result a)
resultToServantErr (ResOk a) = return (ResOk a)
resultToServantErr err@(ResErr status phrase _) =
  left  ServantErr { errHTTPCode = status
                   , errReasonPhrase = phrase
                   , errBody = encode err
                   , errHeaders = [ ("Content-Type", "application/json") ]
                   }
