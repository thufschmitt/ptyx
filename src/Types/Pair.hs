{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types.Pair
  ( T(..)
  , atomic
  ) where

import qualified Data.Bool.Applicative as ABool
import           Data.Semigroup ((<>))
import qualified Data.Set as Set
import qualified Text.ShowM as ShowM
import qualified Types.Bdd as Bdd
import qualified Types.Node as Node
import           Types.SetTheoretic

data Atom t = Atom { first, second :: t }
  deriving (Eq, Ord)

instance ShowM.ShowM m t => ShowM.ShowM m (Atom t) where
  showM (Atom hd tl) = do
    prettyT1 <- ShowM.showM hd
    prettyT2 <- ShowM.showM tl
    pure $ "(" <> prettyT1 <> ")::(" <> prettyT2 <> ")"

newtype T t = T (Bdd.T (Atom t))
  deriving (Eq, Ord, SetTheoretic_)

instance ShowM.ShowM m t => ShowM.ShowM m (T t) where
  showM (T x) = do
    prettyX <- ShowM.showM x
    case prettyX of
      "⊥" -> pure "⊥"
      tt -> pure $ "(" <> tt <> ") & [⊤*]"

-- | Build an atomic pair type
atomic :: t -> t -> T t
atomic hd tl = T (Bdd.atom $ Atom hd tl)

isEmptyL
  :: forall c t m.
     (SetTheoretic c t, c m, Applicative m)
  => T t
  -> m Bool
isEmptyL (T l)
  | Bdd.isTriviallyEmpty l = pure True
  | Bdd.isTriviallyFull l = pure False
  | otherwise =
    let pairs = Bdd.toDNF l in
    ABool.all emptyIntersect pairs
  where
    emptyIntersect :: (Set.Set (Atom t), Set.Set (Atom t)) -> m Bool
    emptyIntersect (posAtoms, negAtoms) =
      let
        capFirsts = capN . Set.map first
        capSeconds = capN . Set.map second
        cupFirsts = cupN . Set.map first
        cupSeconds = cupN . Set.map second
      in
      (capFirsts posAtoms `sub` cupFirsts negAtoms) ABool.&&
      (capSeconds posAtoms `sub` cupSeconds negAtoms) ABool.&&
        forallStrictSubset
          (\subset compl ->
            (capFirsts posAtoms `sub` cupFirsts subset) ABool.||
            (capSeconds posAtoms `sub` cupFirsts compl)
          )
          negAtoms

instance SetTheoretic Node.MemoMonad t => SetTheoretic Node.MemoMonad (T t) where
  isEmpty = isEmptyL
