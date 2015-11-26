{-# LANGUAGE DeriveGeneric              #-}
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
import           Database.HDBC.Query.TH
import qualified Database.HDBC.Record.Insert        as Insert
import           Database.HDBC.Record.Query
import           Database.HDBC.Record.Statement
import           Database.HDBC.Record.TH
import qualified Database.HDBC.Record.Update        as Update
import           Database.HDBC.SqlValue
import           Database.HDBC.Types                (IConnection)
import           Database.Record.Persistable
import           Database.Relational.Query
import           GHC.Generics
import           Language.Haskell.TH.Name.CamelCase

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
deriving instance ShowConstantTermsSQL Variation

instance Convertible SqlValue ExperimentId where
  safeConvert s = ExperimentId <$> safeConvert s
instance Convertible ExperimentId SqlValue where
  safeConvert (ExperimentId s) = safeConvert s
$(derivePersistableInstanceFromValue [t| ExperimentId |])
deriving instance PersistableWidth ExperimentId
deriving instance ShowConstantTermsSQL ExperimentId

instance Convertible SqlValue UserId where
  safeConvert s = UserId <$> safeConvert s
instance Convertible UserId SqlValue where
  safeConvert (UserId s) = safeConvert s
$(derivePersistableInstanceFromValue [t| UserId |])
deriving instance PersistableWidth UserId
deriving instance ShowConstantTermsSQL UserId

instance Convertible SqlValue Reward where
  safeConvert s = Reward <$> safeConvert s
instance Convertible Reward SqlValue where
  safeConvert (Reward s) = safeConvert s
$(derivePersistableInstanceFromValue [t| Reward |])
deriving instance PersistableWidth Reward
deriving instance ShowConstantTermsSQL Reward

-- Table definition for relational record.
$(defineTableDefault
  defaultConfig
  -- schema name
  "bandits"
  -- table name
  "assignment"
  -- columns
  [ ("as_experiment_id", [t| ExperimentId |])
  , ("as_user_id", [t| UserId |])
  , ("as_variation", [t| Variation |])
  , ("as_reward", [t| Reward |])
  ]
  -- derivings
  [toConName "Eq", toConName "Show", toConName "Generic"]
  -- primary key columns
  [0, 1]
  -- not null column?
  Nothing
 )

-- | Hides the presence of the HDBC connection.
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
      insertAssignment' eid uid undefined conn
      k undefined
    run (NewReward eid uid rew k) = do
      updateReward eid uid rew conn
      k

-- | Queries the database
queryAssignment :: ExperimentId -> UserId -> RunHRRBackend (Maybe Variation)
queryAssignment eid uid conn = do
  ps <- prepare conn selectAssignment
  es <- execute (bind ps (eid, uid))
  ma <- fetchUnique' es
  return $ asVariation <$> ma

updateReward :: ExperimentId -> UserId -> Reward -> RunHRRBackend ()
updateReward eid uid rew conn = do
  ps <- Update.prepareUpdate conn upd
  _  <- Update.runPreparedUpdate ps ()
  return ()
  where
    upd =
      typedUpdate tableOfAssignment . updateTarget $ \proj -> do
        asReward' <-# value rew
        wheres $ proj ! asExperimentId' .=. value eid
        wheres $ proj ! asUserId' .=. value uid
        wheres $ proj ! asReward' .=. value (Reward 0.0)

insertAssignment' :: ExperimentId -> UserId -> Variation -> RunHRRBackend ()
insertAssignment' eid uid var conn = do
  ps <- Insert.prepareInsert conn insertAssignment
  _  <- Insert.runPreparedInsert ps Assignment { asExperimentId = eid
                                               , asUserId = uid
                                               , asVariation = var
                                               , asReward = Reward 0.0
                                               }
  return ()
