module Typer.Environ where

import qualified Typer.Environ.Gamma as Gamma
import           Data.Default        (Default, def)

newtype T = T { gamma :: Gamma.T }

instance Default T where
  def = T { gamma = def }
