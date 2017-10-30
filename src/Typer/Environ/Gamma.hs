{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Typer.Environ.Gamma ( T, insert, lookup ) where

import Prelude hiding (map, lookup)
import Data.Default (Default, def)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Types

import Types.SetTheoretic (full)

newtype T = T { getMap :: Map.Map Text Types.T }
  deriving (Monoid)

map :: (Map.Map Text Types.T -> Map.Map Text Types.T) -> T -> T
map f (T x) = T $ f x

insert :: Text -> Types.T -> T -> T
insert name val = map (Map.insert name val)

lookup :: Text -> T -> Maybe Types.T
lookup v = Map.lookup v . getMap

instance Default T where
  def = T $ Map.fromList [("undefined", full)]
