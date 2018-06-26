{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Distinguished
  ( T(..), Atom(..)
  , nil
  ) where

import qualified Control.Monad.State as SM
import qualified Data.Set as Set
import           Types.SetTheoretic

data Atom
  = Nil
  deriving (Eq, Ord, Enum, Bounded)

newtype T = T (Set.Set Atom)
  deriving (Eq, Ord, SetTheoretic_, SetTheoretic (SM.MonadState ()))

atomic :: Atom -> T
atomic = T . Set.singleton

nil :: T
nil = atomic Nil
