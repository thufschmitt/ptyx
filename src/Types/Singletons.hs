module Types.Singletons where

import qualified Types
import qualified Types.Intervals as Intervals

int :: Integer -> Types.T
int i = Types.int $ Intervals.bounded i i
