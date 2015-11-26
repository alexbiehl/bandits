{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Bandits.Api where

import           Bandits.Experiment.Types

import           Control.Monad.Trans.Either
import           Data.Aeson                 hiding (Result)
import           Data.ByteString            (ByteString)
import           Data.Proxy
import           Data.Text                  (Text)
import           Servant.API
import           Servant.Server

type Assign = "experiments"
           :> Capture "experiment" ExperimentId
           :> "assign"
           :> Capture "user" UserId
           :> Get '[JSON] (Result Variation)

type Success = "experiments"
            :> Capture "experiment" ExperimentId
            :> "success"
            :> Capture "user" UserId
            :> Get '[JSON] (Result ())

type MkExperiment = "experiments"
                 :> "create"
                 :> ReqBody '[JSON] Experiment
                 :> Post '[JSON] (Result Experiment)

type ExperimentApi = Assign
                :<|> Success

type BanditsApi = ExperimentApi
             :<|> "static" :> Raw

banditsApi :: Proxy BanditsApi
banditsApi = Proxy

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
