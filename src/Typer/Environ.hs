{-# LANGUAGE OverloadedStrings #-}

module Typer.Environ where

import           Data.Default (Default, def)
import           Data.Text (Text)
import qualified Typer.Environ.Gamma as Gamma
import qualified Typer.Environ.TypeMap as TypeMap
import qualified Types
import           Types.Intervals ()
import           Types.SetTheoretic (empty, full)

data T = T { gamma :: Gamma.T, typeMap :: TypeMap.T }

instance Default T where
  def = T { gamma = def, typeMap = def }

mapGamma :: (Gamma.T -> Gamma.T) -> T -> T
mapGamma f t = t { gamma = f $ gamma t }

addVariable :: Text -> Types.Node -> T -> T
addVariable name typ = mapGamma (Gamma.insert name typ)

lookupVariable :: Text -> T -> Maybe Types.Node
lookupVariable name = Gamma.lookup name . gamma

getType :: T -> Text -> Maybe Types.Node
getType env name = TypeMap.lookup name (typeMap env)
