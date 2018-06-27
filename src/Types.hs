{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module: Types
Description: Tix used by Tix

-}
module Types
  ( T
  , Node
  , arrows
  , ints
  , arrow
  , int
  , bool
  , dist
  , pair
  , undef
  ) where


import qualified Control.Monad.Memo as Memo
import qualified Data.Bool.Applicative as ABool
import qualified Data.Text.Lazy as T
import           Prelude
import qualified Text.ShowM as ShowM
import qualified Types.Arrow as Arrow
import qualified Types.Bool as Bool
import qualified Types.Distinguished as Distinguished
import qualified Types.Intervals as Intervals
import qualified Types.Node as Node
import qualified Types.Pair as Pair
import           Types.SetTheoretic

-- | A type is represented by a record whose fields corresponds to its
-- projections on the various kinds. So a type is the union of its fields.
data T = T
  {
  -- | The arrow part of the type
  arrows :: Arrow.T Node
  -- | The integral part of the type
  , ints :: Intervals.T
  , bools :: Bool.T
  , pairs :: Pair.T Node
  , distinguished :: Distinguished.T
  }
  deriving (Eq, Ord)

type Node = Node.T T

instance ShowM.ShowM Memo.T T where
  showM t@T{arrows, ints, bools, pairs, distinguished}
    | Memo.runEmpty $ isEmpty t = pure "⊥"
    | Memo.runEmpty $ isFull t = pure "⊤"
    | otherwise = T.intercalate " | " . filter (not . (==) "⊥") <$>
      sequenceA
      [ ShowM.showM arrows
      , ShowM.showM ints
      , ShowM.showM bools
      , ShowM.showM pairs
      , ShowM.showM distinguished
      ]

instance Show T where
  show = T.unpack . Memo.runEmpty . ShowM.showM

map2 :: (forall t. SetTheoretic_ t => t -> t -> t)
       -> T -> T -> T
map2 f t1 t2 = T
  { arrows = f (arrows t1) (arrows t2)
  , ints = f (ints t1) (ints t2)
  , bools = f (bools t1) (bools t2)
  , pairs = f (pairs t1) (pairs t2)
  , distinguished = f (distinguished t1) (distinguished t2)
  }

instance SetTheoretic_ T where
  empty = T
    { arrows = empty
    , ints = empty
    , bools = empty
    , pairs = empty
    , distinguished = empty
    }

  full = T
    { arrows = full
    , ints   = full
    , bools = full
    , pairs = full
    , distinguished = full
    }

  cup = map2 cup
  cap = map2 cap
  diff = map2 diff

instance SetTheoretic T where
  sub t1 t2 =
    sub (arrows t1) (arrows t2) ABool.&&
    sub (ints t1) (ints t2) ABool.&&
    sub (bools t1) (bools t2) ABool.&&
    sub (pairs t1) (pairs t2) ABool.&&
    sub (distinguished t1) (distinguished t2)

arrow :: Arrow.T Node -> T
arrow a = empty { arrows = a }

int :: Intervals.T -> T
int i = empty { ints = i }

bool :: Bool.T -> T
bool b = empty { bools = b }

pair :: Pair.T Node -> T
pair l = empty { pairs = l }

dist :: Distinguished.T -> T
dist d = empty { distinguished = d }

undef :: T
undef = full
