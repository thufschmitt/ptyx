{-|
Description: Arrow types
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types.Arrow (
  T(..), Arrow(..),
  domain, codomain,
  )
where

import Types.SetTheoretic

import qualified Types.Bdd as Bdd
import qualified Types.DNF as DNF

import Data.Monoid ((<>))
import qualified Data.Set as Set

-- | Atomic arrow type
data Arrow t = Arrow t t deriving (Eq, Ord, Show)

-- | Arrow type
newtype T t = T (Bdd.T (Arrow t)) deriving (Eq, Ord, Show, SetTheoretic_)

-- | Returns the domain of an atomic arrow type
domain :: Arrow t -> t
domain (Arrow d _) = d

-- | Returns the codomain of an atomic arrow type
codomain :: Arrow t -> t
codomain (Arrow _ c) = c

isEmptyA :: SetTheoretic t => T t -> Bool
isEmptyA (T a)
  | Bdd.isTriviallyEmpty a = True
  | Bdd.isTriviallyFull a = False
  | otherwise =
    let arrow = Bdd.get a in
    all emptyIntersect arrow

    where
      emptyIntersect (pos, neg) =
        any (sub' pos) neg

      sub' p (Arrow t1 t2) =
        subCupDomains t1 p &&
        superCapCodomains t2 p &&
        forallStrictSubset
          (\subset comp -> subCupDomains t1 subset || superCapCodomains t1 comp)
          (Set.fromList p)

      subCupDomains t p =
        sub (cupN $ fmap domain p) t

      superCapCodomains t p =
        sub t (capN $ fmap codomain p)

      forallStrictSubset f elts = forallStrictSubset' f elts Set.empty

      forallStrictSubset' f elts removedElts
        | Set.null elts = True
        | otherwise =
          let
            directsubsets =
                          [ (Set.delete x elts, Set.insert x removedElts)
                          | x <- Set.toList elts ]
          in
          all (uncurry $ forallStrictSubset' f) directsubsets

instance SetTheoretic t => SetTheoretic (T t) where
  isEmpty = isEmptyA

apply :: forall t. SetTheoretic t => DNF.T (Arrow t) -> t -> t
apply arrows arg =
  DNF.foldl1 (\accu arr -> applyIntersection arr `cup` accu) empty arrows
  where
    applyIntersection :: Set.Set (Arrow t) -> t
    applyIntersection arrows =
      let
        allValidSubsets =
          filter
            (\sset -> not $ arg `sub` cupN (domain <$> Set.toList sset))
            (subsets arrows)
      in
      cupN $
        map
          (\sset -> capN $ codomain <$> Set.toList (Set.difference arrows sset))
          allValidSubsets
    subsets :: Ord a => Set.Set a -> [Set.Set a]
    subsets = foldl
      (\accu elt -> accu <> map (Set.insert elt) accu)
      []
