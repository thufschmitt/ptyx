module Types.Singletons where

import qualified Types
import qualified Types.Intervals as Intervals
import qualified Types.Arrow as Arrow

int :: Integer -> Types.T
int i = Types.int $ Intervals.bounded i i
