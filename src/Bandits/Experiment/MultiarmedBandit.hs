module Bandits.Experiment.MultiarmedBandit where

-- | Bandits have arms. Mutiple of them!
type Arm = Int

-- | A reward for an arm in an experiment.
type Reward = Float

data Bandit m = Bandit { runChoose :: m Arm
                         -- ^ Chooses an arm from the bandit.
                       , runUpdate :: Arm -> Reward -> m ()
                         -- ^ Rewards an arm.
                       }
