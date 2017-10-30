{-|
Description: Arrow types
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types.Arrow (
  T(..), Arrow(..),
  domain, codomain,
  getApplication
  )
where

import Types.SetTheoretic

import qualified Types.Bdd as Bdd

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

-- | @getApplication arr s@ returns the smaller arrow atom type @s -> t@ such
-- that @s -> t <: arr@
getApplication :: forall t. SetTheoretic t => T t -> t -> Arrow t
getApplication (T arr) s = Bdd.foldBdd
  Bdd.FoldParam{
    Bdd.fpEmpty = Arrow empty full,
    Bdd.fpFull = Arrow full empty,
    Bdd.fpAtom = \(Arrow dom codom) ->
      Arrow (dom `cap` s) codom,
    Bdd.fpCup = \(Arrow t1 t2) (Arrow u1 u2) ->
      Arrow (cap t1 u1) (cup t2 u2),
    Bdd.fpCap = \(Arrow t1 t2) (Arrow u1 u2) ->
      let
        s1 = diff t1 u1
        s2 = diff u1 t1
      in
      if isEmpty s1
      then Arrow t1 t2
      else if isEmpty s2
      then Arrow u1 u2
      else Arrow (t1 `cup` u1) (t2 `cup` u2),
    Bdd.fpDiff = const -- FIXME: this is not what we want
  }
  arr
