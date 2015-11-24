{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Bandits.Api where

import           Bandits.Experiment.Types

import           Data.Proxy
import           Servant.API

type Assign = "experiments"
           :> Capture "experiment" ExperimentId
           :> "assign"
           :> Capture "user" UserId
           :> Get '[JSON] Variation

type Success = "experiments"
            :> Capture "experiment" ExperimentId
            :> "success"
            :> Capture "user" UserId
            :> Get '[JSON] ()

type ExperimentApi = Assign
                :<|> Success

type BanditsApi = ExperimentApi
             :<|> "static" :> Raw

banditsApi :: Proxy BanditsApi
banditsApi = Proxy
