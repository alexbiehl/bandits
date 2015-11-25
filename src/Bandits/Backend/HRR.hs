{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
module Bandits.Backend.HRR where

import           Bandits.Experiment.Instructions
import           Bandits.Experiment.Types

import           Control.Monad.Free
import           Data.Text                       (Text)
import           Database.HDBC.Query.TH
import           Database.HDBC.Record.Query
import           Database.HDBC.Record.Statement
import           Database.HDBC.Types             (IConnection)
import           Database.Relational.Query

-- Table definition for relational record.
$(defineTableDefault
  defaultConfig
  "bandits"
  "assignment"
  [ ("as_experiment_id", [t| Text  |])
  , ("as_user_id", [t| Text |])
  , ("as_variation", [t| Text |])
  ]
  []
  [0, 1]
  Nothing
 )

-- | Abstracts the presence of the HDBC connection.
type RunHRRBackend a = forall c. IConnection c => c -> IO a

-- | Runs an experiment in the Free monad constructor.
runExperiment :: Free ExpInstr a -> RunHRRBackend a
runExperiment m conn = iterM run m
  where
    run :: ExpInstr (IO a) -> IO a
    run (LookupAssignment eid uid k) = do
      a <- queryAssignment eid uid conn
      k a
    run (NewAssignment eid uid k) = do
      undefined
    run (NewReward eid uid rew k) = do
      undefined

-- | Queries the database
queryAssignment :: ExperimentId -> UserId -> RunHRRBackend (Maybe Variation)
queryAssignment (ExperimentId eid) (UserId uid) conn = do
  ps <- prepare conn selectAssignment
  es <- execute (bind ps (eid, uid))
  ma <- fetchUnique' es
  return $ (Variation . asVariation) <$> ma
