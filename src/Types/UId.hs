{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- This module defines unique identifiers used in the definition of
-- 'Types.Node.T'
module Types.UId
  ( T
  , GenT
  , MonadGen
  , runGenT
  , fresh
  ) where

import qualified Control.Monad.Random.Class as RC
import qualified Control.Monad.Random.Strict as MR
import qualified Control.Monad.State as S
import           Data.Coerce (coerce)
import qualified Data.UUID as U
import qualified System.Random as R

type T = U.UUID

type GenT = MR.RandT R.StdGen

type MonadGen = RC.MonadRandom

runGenT :: Monad m => GenT m a -> Int -> m a
runGenT r seed = MR.evalRandT r (R.mkStdGen seed)

fresh :: MonadGen m => m T
fresh = MR.getRandom
