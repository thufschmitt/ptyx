{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module: Types
Description: Tix used by Tix

-}
module Types
  ( T
  , arrows
  , ints
  , arrow
  , int
  , bool
  , undef
  ) where


import Prelude
import Data.List (intercalate)
import Types.SetTheoretic
import qualified Types.Arrow as Arrow
import qualified Types.Bool as Bool
import qualified Types.Intervals as Intervals

-- | A type is represented by a record whose fields corresponds to its
-- projections on the various kinds. So a type is the union of its fields.
data T = T {
             -- | The arrow part of the type
             arrows :: Arrow.T T
             -- | The integral part of the type
           , ints :: Intervals.T
           , bools :: Bool.T
           }
           deriving (Eq, Ord)

instance Show T where
  show t@T{arrows, ints, bools}
    | isEmpty t = "⊥"
    | isFull t = "⊤"
    | otherwise = intercalate " | " $ filter (not . (==) "⊥")
      [show arrows, show ints, show bools]

map2 :: (forall t. SetTheoretic t => t -> t -> t)
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

instance SetTheoretic T where
  isEmpty x = sub x empty
  sub t1 t2 =
    sub (arrows t1) (arrows t2) &&
    sub (ints t1) (ints t2) &&
    sub (bools t1) (bools t2)

arrow :: Arrow.T T -> T
arrow a = empty { arrows = a }

int :: Intervals.T -> T
int i = empty { ints = i }

bool :: Bool.T -> T
bool b = empty { bools = b }

undef :: T
undef = full
