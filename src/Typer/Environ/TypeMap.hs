{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Typer.Environ.TypeMap ( T(T), insert, lookup, map, getMap ) where

import           Data.Default (Default, def)
import qualified Data.Map as Map
import           Data.Text (Text)
import           Prelude hiding (lookup, map)
import qualified Types
import qualified Types.Arrow as Arrow
import qualified Types.Node as Node
import qualified Types.Singletons as S

import           Types.SetTheoretic (empty, full, neg, (/\))

newtype T = T { getMap :: Map.Map Text Types.Node }
  deriving (Monoid)

map :: (Map.Map Text Types.Node -> Map.Map Text Types.Node) -> T -> T
map f (T x) = T $ f x

insert :: Text -> Types.Node -> T -> T
insert name val = map (Map.insert name val)

lookup :: Text -> T -> Maybe Types.Node
lookup v = Map.lookup v . getMap

instance Default T where
  def = T $ Map.fromList
    [ ("Int", pure $ Types.int full)
    , ("Bool", pure $ Types.bool full)
    , ("Any", pure  full)
    , ("Empty", pure empty)
    ]
