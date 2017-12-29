module Types.Singletons where

import qualified Types
import qualified Types.Intervals as Intervals
import qualified Types.Bool as Bool

int :: Integer -> Types.T
int i = Types.int $ Intervals.bounded i i

bool :: Bool -> Types.T
bool True = Types.bool Bool.TrueT
bool False = Types.bool Bool.FalseT
