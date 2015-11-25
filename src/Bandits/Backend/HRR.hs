{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Bandits.Backend.HRR where

import           Bandits.Experiment.Instructions
import           Bandits.Experiment.Types

import           Control.Monad.Free
import           Data.Convertible
import           Data.Text                       (Text)
import           Database.HDBC.Query.TH
import           Database.HDBC.Record.Query
import           Database.HDBC.Record.Statement
import           Database.HDBC.Record.TH
import           Database.HDBC.SqlValue
import           Database.HDBC.Types             (IConnection)
import           Database.Record.Persistable
import           Database.Relational.Query

-- We need some specific instances for HDBC.
-- as we don't want to leak HDBC in our core logic
-- we define orphan instances here. As we have
-- Multiparamtypeclasses here we cannot use
-- standalonederiving.
instance Convertible SqlValue Variation where
  safeConvert s = Variation <$> safeConvert s
instance Convertible Variation SqlValue where
  safeConvert (Variation s) = safeConvert s
$(derivePersistableInstanceFromValue [t| Variation |])

deriving instance PersistableWidth Variation
instance Convertible SqlValue ExperimentId where
  safeConvert s = ExperimentId <$> safeConvert s
instance Convertible ExperimentId SqlValue where
  safeConvert (ExperimentId s) = safeConvert s
$(derivePersistableInstanceFromValue [t| ExperimentId |])

deriving instance PersistableWidth ExperimentId
instance Convertible SqlValue UserId where
  safeConvert s = UserId <$> safeConvert s
instance Convertible UserId SqlValue where
  safeConvert (UserId s) = safeConvert s
$(derivePersistableInstanceFromValue [t| UserId |])
deriving instance PersistableWidth UserId

-- Table definition for relational record.
$(defineTableDefault
  defaultConfig
  "bandits"
  "assignment"
  [ ("as_experiment_id", [t| ExperimentId |])
  , ("as_user_id", [t| UserId |])
  , ("as_variation", [t| Variation |])
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
queryAssignment eid uid conn = do
  ps <- prepare conn selectAssignment
  es <- execute (bind ps (eid, uid))
  ma <- fetchUnique' es
  return $ asVariation <$> ma
