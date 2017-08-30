{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.Arrow where

import Types.SetTheoretic

import qualified Types.Bdd as Bdd

import qualified Data.Set as Set

data Arrow t = Arrow t t deriving (Eq, Ord, Show)

newtype T t = T (Bdd.Bdd (Arrow t)) deriving (Eq, Ord, Show, SetTheoretic_)

domain :: Arrow t -> t
domain (Arrow d _) = d

codomain :: Arrow t -> t
codomain (Arrow _ c) = c

isEmptyA :: SetTheoretic t => T t -> Bool
isEmptyA (T (Bdd.Leaf l)) = not l

isEmptyA (T a) =
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
