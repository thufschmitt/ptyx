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
  , undef
  ) where


import qualified Data.Text.Lazy as T
import           Prelude
import qualified Text.ShowM as ShowM
import qualified Types.Arrow as Arrow
import qualified Types.Bool as Bool
import qualified Types.Intervals as Intervals
import qualified Types.Node as Node
import           Types.SetTheoretic

-- | A type is represented by a record whose fields corresponds to its
-- projections on the various kinds. So a type is the union of its fields.
data T = T {
             -- | The arrow part of the type
             arrows :: Arrow.T Node
             -- | The integral part of the type
           , ints :: Intervals.T
           , bools :: Bool.T
           }
           deriving (Eq, Ord)

type Node = Node.T T

instance ShowM.ShowM Node.Memo T where
  showM t@T{arrows, ints, bools}
    | Node.run mempty $ isEmpty t = pure "⊥"
    | Node.run mempty $ isFull t = pure "⊤"
    | otherwise = T.intercalate " | " . filter (not . (==) "⊥") <$>
      sequenceA [ShowM.showM arrows, ShowM.showM ints, ShowM.showM bools]

instance Show T where
  show = T.unpack . Node.runEmpty . ShowM.showM

map2 :: (forall t. SetTheoretic_ t => t -> t -> t)
       -> T -> T -> T
map2 f t1 t2 = T { arrows = f (arrows t1) (arrows t2)
                 , ints = f (ints t1) (ints t2)
                 , bools = f (bools t1) (bools t2)
                 }

instance SetTheoretic_ T where
  empty = T { arrows = empty
            , ints = empty
            , bools = empty
            }

  full = T { arrows = full
           , ints   = full
           , bools = full
           }

  cup = map2 cup
  cap = map2 cap
  diff = map2 diff

instance SetTheoretic Node.MemoMonad T where
  sub t1 t2 =
    sub (arrows t1) (arrows t2) <&&>
    sub (ints t1) (ints t2) <&&>
    sub (bools t1) (bools t2)

arrow :: Arrow.T Node -> T
arrow a = empty { arrows = a }

int :: Intervals.T -> T
int i = empty { ints = i }

bool :: Bool.T -> T
bool b = empty { bools = b }

undef :: T
undef = full
