{-| Description of formulas in disjunctive normal form -}

module Types.DNF where

import qualified Types.Bdd as Bdd
import qualified Data.Set as Set

import Data.Monoid ((<>))

newtype T a = T [Set.Set a]
  deriving (Eq, Ord, Show)

foldl1 :: (b -> Set.Set a -> b) -> b -> T a -> b
foldl1 f init (T l) = foldl f init l

fromBdd :: Ord a => Bdd.T a -> T a
fromBdd = T .
  Bdd.foldBdd Bdd.FoldParam{
    Bdd.fpEmpty = [],
    Bdd.fpFull = [Set.empty],
    Bdd.fpAtom = \x -> [Set.singleton x],
    Bdd.fpCup = (<>),
    Bdd.fpCap = \x y ->
      foldl
        (\acc1 elt1 ->
          foldl
            (\acc2 elt2 -> (elt1 <> elt2) : acc2)
            acc1
            y)
        mempty
        x,
    Bdd.fpDiff = error "FIXME: unimplemented"
  }
