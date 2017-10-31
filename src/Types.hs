{-|
Module: Types
Description: Tix used by Tix

-}
module Types (T, arrows, ints, arrow, int, undef) where


import Prelude hiding (any)
import Types.SetTheoretic
import qualified Types.Arrow as Arrow
import qualified Types.Intervals as Intervals

-- | A type is represented by a record whose fields corresponds to its
-- projections on the various kinds. So a type is the union of its fields.
data T = T {
             -- | The arrow part of the type
             arrows :: Arrow.T T
             -- | The integral part of the type
           , ints :: Intervals.T
           }
           deriving (Eq, Ord, Show)

map2 :: (Arrow.T T -> Arrow.T T -> Arrow.T T)
       -> (Intervals.T -> Intervals.T -> Intervals.T)
       -> T -> T -> T
map2 fA fI t1 t2 = T { arrows = fA (arrows t1) (arrows t2)
                     , ints = fI (ints t1) (ints t2)
                     }

instance SetTheoretic_ T where
  empty = T { arrows = empty
            , ints = empty
            }

  full = T { arrows = full
           , ints   = full
           }

  cup = map2 cup cup
  cap = map2 cap cap
  diff = map2 diff diff

instance SetTheoretic T where
  sub t1 t2 =
    sub (arrows t1) (arrows t2) &&
    sub (ints t1) (ints t2)

arrow :: Arrow.T T -> T
arrow a = T { ints = empty, arrows = a }

int :: Intervals.T -> T
int i = T { ints = i, arrows = empty }

undef :: T
undef = full
