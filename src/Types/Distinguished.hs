{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types.Distinguished
  ( T(..), Atom(..)
  , nil
  ) where

import qualified Control.Monad.State as SM
import qualified Data.Set as Set
import qualified Text.ShowM as ShowM
import           Types.SetTheoretic

data Atom
  = Nil
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Monad m => ShowM.ShowM m Atom where showM = ShowM.fromShow

newtype T = T (Set.Set Atom)
  deriving (Eq, Ord, SetTheoretic_, SetTheoretic)

deriving instance Monad m => ShowM.ShowM m T

atomic :: Atom -> T
atomic = T . Set.singleton

nil :: T
nil = atomic Nil
