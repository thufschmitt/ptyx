{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- This module defines unique identifiers used in the definition of
-- 'Types.Node.T'
module Types.UId
  ( T
  , MonadGen
  , fresh
  ) where

import qualified Control.Monad.Random.Class as RC
import qualified Control.Monad.Random.Strict as MR
import qualified Control.Monad.State as S
import           Data.Coerce (coerce)
import qualified Data.UUID as U
import qualified System.Random as R

type T = Int

class Monad m => MonadGen m where
  fresh :: m T
