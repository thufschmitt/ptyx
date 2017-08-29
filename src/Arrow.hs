{-# LANGUAGE ScopedTypeVariables #-}

module Arrow where

import qualified Bdd

import Data.Set (Set)
import qualified Data.Set as Set

data Arrow = Arrow T T deriving (Eq, Ord)

data T = T { arrows :: Bdd.BddT Arrow
           , ints :: Set Int
           } deriving (Eq, Ord)

anyInt :: Set Int
anyInt = Set.fromList [0..10]

domain :: Arrow -> T
domain (Arrow d _) = d

empty :: T
empty = T { arrows = Bdd.empty, ints = Set.empty }

full :: T
full = T { arrows = Bdd.full, ints = anyInt }

codomain :: Arrow -> T
codomain (Arrow _ c) = c

cupT :: T -> T -> T
cupT t1 t2 =
  T { arrows = Bdd.cup (arrows t1) (arrows t2)
    , ints = Set.union (ints t1) (ints t2)
    }

capT :: T -> T -> T
capT t1 t2 =
  T { arrows = Bdd.cap (arrows t1) (arrows t2)
    , ints = Set.intersection (ints t1) (ints t2)
    }

diffT :: T -> T -> T
diffT t1 t2 =
  T { arrows = Bdd.diff (arrows t1) (arrows t2)
    , ints = Set.difference (ints t1) (ints t2)
    }

cupN :: [T] -> T
capN :: [T] -> T

cupN = foldl cupT empty
capN = foldl capT full


isEmpty :: T -> Bool
isEmpty t = isEmptyA (arrows t) && Set.isSubsetOf (ints t) Set.empty

isEmptyA :: Bdd.BddT Arrow -> Bool
isEmptyA (Bdd.Leaf l) = not l

isEmptyA a =
  let arrow :: [([Arrow], [Arrow])] = Bdd.get a in
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
      | otherwise     =
        let directsubsets = [ (Set.delete x elts, Set.insert x removedElts) | x <- Set.toList elts ] in
        all (uncurry $ forallStrictSubset' f) directsubsets

sub :: T -> T -> Bool
sub t1 t2 = isEmpty (diffT t1 t2)
