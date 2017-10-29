{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Typer.Environ.Gamma ( T, insert ) where

import Prelude hiding (map)
import Data.Default (Default, def)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Types

newtype T = T (Map.Map Text Types.T)
  deriving (Monoid)

map :: (Map.Map Text Types.T -> Map.Map Text Types.T) -> T -> T
map f (T x) = T $ f x

insert :: Text -> Types.T -> T -> T
insert name val = map (Map.insert name val)

instance Default T where
  def = T Map.empty
