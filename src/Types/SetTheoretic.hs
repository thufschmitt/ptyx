{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-|
Description: typeclasses for set-theoretic datatypes

Provides set-theoretic connectives and related operations
-}
module Types.SetTheoretic where

import qualified Control.Monad.State as SM
import qualified Data.Bool.Applicative as ABool
import qualified Data.Set as Set

-- | Typeclass for types with set-theoretic (unions, intersections, ...)
-- operations
class Ord a => SetTheoretic_ a where

  -- | The empty set
  empty :: a

  -- | The maximal set that contains all the other sets
  full :: a

  -- | Set-theoretic operations
  cup :: a -> a -> a
  cap :: a -> a -> a
  diff :: a -> a -> a
  neg :: a -> a

  -- | Infix version of the operators
  (\/) :: a -> a -> a
  (/\) :: a -> a -> a
  (\\) :: a -> a -> a

  (\/) = cup
  (/\) = cap
  (\\) = diff

  neg = diff full
  -- diff x y = x \\ neg y

-- | N-ary versions of the set-theoretic operators
cupN :: (SetTheoretic_ a, Foldable t) => t a -> a
capN :: (SetTheoretic_ a, Foldable t) => t a -> a
cupN = foldl cup empty
capN = foldl cap full

-- | SetTheoretic with tests for emptyness and containment
--
-- One may be automatically defined in term of the other
class (SetTheoretic_ a) => SetTheoretic c a | a -> c where
  isEmpty :: c m => a -> m Bool
  sub :: c m => a -> a -> m Bool

  sub x1 x2 = isEmpty $ diff x1 x2
  isEmpty x = sub x empty

  {-# MINIMAL isEmpty | sub #-}

-- | Infix version of sub
(<:) :: (SetTheoretic (SM.MonadState x) a, Monoid x) => a -> a -> Bool
a <: b = flip SM.evalState mempty $ a `sub` b

isFull :: (SetTheoretic c a, c m) => a -> m Bool
isFull x = isEmpty (full \\ x)

(~:) :: (SetTheoretic (SM.MonadState x) a, Monoid x) => a -> a -> Bool
a ~: b = flip SM.evalState mempty $ (a `sub` b) ABool.&& (b `sub` a)

instance (Ord a, Enum a, Bounded a) => SetTheoretic_ (Set.Set a) where
  empty = Set.empty
  full = Set.fromList [minBound..maxBound]
  cup = Set.union
  cap = Set.intersection
  diff = Set.difference

instance (Ord a, Enum a, Bounded a) => SetTheoretic (SM.MonadState ()) (Set.Set a) where
  isEmpty = pure . Set.null
  sub x y = pure $ Set.isSubsetOf x y

-- |
-- Performs a 'fold' over all the (strict) subsets of the given set
foldStrictSubsets
  :: forall a b.
     Ord a
  => b
  -> (b -> Set.Set a -> Set.Set a -> b)
  -> Set.Set a
  -> Set.Set a
  -> b
foldStrictSubsets fi f e= aux fi e
  where
    aux :: b -> Set.Set a -> Set.Set a -> b
    aux foldInit elts removedElts =
      let
        directsubsets =
                      [ (Set.delete x elts, Set.insert x removedElts)
                      | x <- Set.toList elts ]
      in
      foldl
        (\accu (subset, compl) ->
          f
            (aux accu subset compl)
            subset
            compl)
        foldInit
        directsubsets

-- |
-- Checks whether the given property holds for every (strict) subset of the
-- given set
forallStrictSubset
  :: (Ord a, Applicative f)
  => (Set.Set a -> Set.Set a -> f Bool)
  -> Set.Set a
  -> f Bool
forallStrictSubset f =
  foldStrictSubsets
    (pure True)
    (\accu elt compl -> accu ABool.&& f elt compl)
    Set.empty
