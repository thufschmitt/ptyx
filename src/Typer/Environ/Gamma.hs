module Typer.Environ.Gamma ( T ) where

import Data.Default (Default, def)
import qualified Data.Map.Strict as Map
import Data.Text
import qualified Types

newtype T = T (Map.Map Text Types.T)

instance Default T where
  def = T Map.empty
