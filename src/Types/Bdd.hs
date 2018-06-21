{-|
Module: Types.Bdd
Description: Binary decision diagrams

A data structure to represent boolean formulas, with efficient operations of
union and intersection.
Used here to represents set-theoretic combinations of types.
|-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Bdd (
  T,
  atom, isTriviallyFull, isTriviallyEmpty,
  foldBdd, FoldParam(..),
  DNF, toDNF
  )
where

import           Types.SetTheoretic

import           Data.Foldable (foldlM)
import           Data.Semigroup ((<>))
import qualified Data.Set as Set
import qualified Text.ShowM as ShowM

-- | A Binary decision diagram
data T a
    = Leaf Bool
    | Split { tif :: a, tthen :: T a, telse :: T a }
    deriving (Eq, Ord)

instance (Monad m, ShowM.ShowM m a) => ShowM.ShowM m (T a) where
  showM x
    | isTriviallyEmpty x = pure "⊥"
    | isTriviallyFull  x = pure "⊤"
    | otherwise          =
      showDNF $ toListDNF x
      where
        showDNF = foldlM
          (\acc elt -> do
            prettyElt <- showConj elt
            pure $ prettyElt <> " | " <> acc)
          "⊥"
        showConj (posAtoms, negAtoms) = do
          prettyPosAtoms <- showConjPos posAtoms
          prettyNegAtoms <- showConjNeg negAtoms
          pure . parens $ prettyPosAtoms <> " & " <> prettyNegAtoms
        showConjPosNeg discr = foldlM
          (\acc elt -> do
            prettyElt <- ShowM.showM elt
            pure $ discr <> prettyElt <> " & " <> acc)
          "⊤"
        showConjPos = showConjPosNeg ""
        showConjNeg = showConjPosNeg "¬"
        parens elt = "(" <> elt <> ")"

-- | @atom x@ Returns the Bdd containing only the atom @x@
atom :: a -> T a
atom x = Split x (Leaf True) (Leaf False)

-- | Tell wether this is the trivial full Bdd
isTriviallyFull :: T a -> Bool
isTriviallyFull (Leaf True) = True
isTriviallyFull _ = False

-- | Tell wether this is the trivial empty Bdd
isTriviallyEmpty :: T a -> Bool
isTriviallyEmpty (Leaf False) = True
isTriviallyEmpty _ = False

-- | Parameters for the @foldBdd@ function.
-- The existence of this record is just a workaround around the fact that
-- haskell has no labeled arguments.
data FoldParam src target = FoldParam { fpEmpty :: target
                                        , fpFull :: target
                                        , fpCup :: target -> target -> target
                                        , fpCap :: target -> target -> target
                                        , fpDiff :: target -> target -> target
                                        , fpAtom :: src -> target
                                        }

-- | Recursively compute a value from a Bdd
foldBdd :: FoldParam a b -> T a -> b
foldBdd param bdd =
  case bdd of
    Leaf False -> fpEmpty param
    Leaf True -> fpFull param
    Split x p n ->
      let x' = fpAtom param x
          p' = fpCap param x' (foldBdd param p)
          n' = fpDiff param x' (foldBdd param n)
      in
      fpCup param p' n'

instance Ord a => SetTheoretic_ (T a) where
  empty = Leaf False
  full  = Leaf True

  cup (Leaf True) _ = Leaf True
  cup _ (Leaf True) = Leaf True
  cup a (Leaf False) = a
  cup (Leaf False) a = a
  cup b1 b2 =
    let (Split a1 c1 d1) = b1
        (Split a2 c2 d2) = b2
    in
    recurse cup b1 b2 a1 c1 d1 a2 c2 d2

  cap (Leaf False) _ = Leaf False
  cap _ (Leaf False) = Leaf False
  cap a (Leaf True)  = a
  cap (Leaf True) a  = a
  cap b1 b2 =
    let (Split a1 c1 d1) = b1
        (Split a2 c2 d2) = b2
    in
    recurse cap b1 b2 a1 c1 d1 a2 c2 d2

  diff (Leaf False) _ = Leaf False
  diff _ (Leaf True)  = Leaf False
  diff a (Leaf False) = a
  diff (Leaf True) (Split a c d) =
    Split a (diff (Leaf True) c) (diff (Leaf True) d)
  diff b1 b2 =
    let (Split a1 c1 d1) = b1
        (Split a2 c2 d2) = b2
    in
    recurse diff b1 b2 a1 c1 d1 a2 c2 d2

recurse :: Ord a => (t1 -> t -> T a) -> t1 -> t -> a -> t1 -> t1 -> a -> t -> t -> T a
recurse op b1 b2 a1 c1 d1 a2 c2 d2
        | a1 == a2 = Split a1 (op c1 c2) (op d1 d2)
        | a1 < a2 = Split a1 (op c1 b2) (op d1 b2)
        | otherwise = Split a2 (op b1 c2) (op b1 d2)

-- | Disjunctive normal form
-- Alternative representation for boolean formulas, sometime easier to use
--
-- The outer list corresponds to a bid disjunction, and for each element of
-- this list, the first element of the pair is a conjunction of atoms and the
-- second a conjonction of negated atoms.
type DNF a = Set.Set (Set.Set a,Set.Set a)

toDNF :: Ord a => T a -> DNF a
toDNF = aux Set.empty Set.empty Set.empty
  where
    aux :: Ord a => DNF a -> Set.Set a -> Set.Set a -> T a -> DNF a
    aux accu posAtoms negAtoms = \case
      Leaf True -> Set.insert (posAtoms, negAtoms) accu
      Leaf False -> accu
      Split { tif, tthen, telse } ->
        let accuR = aux accu (Set.insert tif posAtoms) negAtoms tthen
            accuRL = aux accuR posAtoms (Set.insert tif negAtoms) telse
        in
        accuRL

toListDNF :: T a -> [([a],[a])]
toListDNF = aux [] [] []
  where
    aux :: [([a],[a])] -> [a] -> [a] -> T a ->  [([a],[a])]
    aux accu posAtom negAtom = \case
      Leaf True -> (posAtom, negAtom) : accu
      Leaf False -> accu
      Split { tif, tthen, telse } ->
        let accuR = aux accu (tif : posAtom) negAtom tthen
            accuRL = aux accuR posAtom (tif : negAtom) telse
        in
        accuRL
