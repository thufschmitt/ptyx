{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Types.Node
  ( T(..)
  , noId
  , new
  ) where

import           Control.Applicative (liftA2)
import qualified Control.Monad.State as SM
import qualified Data.Set as Set
import           Types.SetTheoretic
import qualified Types.UId as UId

data T a = T { typ :: a, id :: Maybe UId.T }
  deriving (Eq, Ord)

instance Show a => Show (T a) where show = show . typ

instance Functor T where
  fmap f (T x _) = noId $ f x

instance Applicative T where
  pure = flip T Nothing
  liftA2 f (T x _) (T y _) = noId $ f x y

instance Monad T where
  (T a _) >>= f = f a

noId :: a -> T a
noId = flip T Nothing

instance SetTheoretic_ a => SetTheoretic_ (T a) where
  empty = pure empty
  full = pure full
  cup = liftA2 cup
  cap = liftA2 cap
  diff = liftA2 diff
  neg = fmap neg

instance SetTheoretic m a => SetTheoretic m (T a) where
  isEmpty (T x _) = isEmpty x
  sub (T x _) (T y _) = sub x y

new :: UId.MonadGen m => a -> m (T a)
new elt = do
  uid <- UId.fresh
  pure $ T elt (Just uid)
