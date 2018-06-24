-- |
-- Extension of various boolean combinations to values of type @f Bool@ for
-- some monad @f@
module Data.Bool.Applicative
  ( all
  , any
  , (&&), (||)
  ) where

import           Control.Applicative (liftA2)
import           Data.List (foldl')
import           Prelude hiding (all, any, (&&), (||))
import qualified Prelude as P

(&&) :: Applicative m => m Bool -> m Bool -> m Bool
(&&) = liftA2 (P.&&)

(||) :: Applicative m => m Bool -> m Bool -> m Bool
(||) = liftA2 (P.||)

-- | Monadic version of 'all'
all :: (Applicative f, Foldable t) => (a -> f Bool) -> t a -> f Bool
all f = foldl' (\acc x -> acc && f x) (pure True)

-- | Monadic version of 'all'
any :: (Applicative f, Foldable t) => (a -> f Bool) -> t a -> f Bool
any f = foldl' (\acc x -> acc || f x) (pure False)
