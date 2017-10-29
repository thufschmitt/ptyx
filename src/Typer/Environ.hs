{-# LANGUAGE OverloadedStrings #-}

module Typer.Environ where

import qualified Typer.Environ.Gamma as Gamma
import           Data.Default        (Default, def)
import Data.Text (Text)
import Types.Intervals ()
import Types.SetTheoretic (empty, full)
import qualified Types

newtype T = T { gamma :: Gamma.T }

instance Default T where
  def = T { gamma = def }

mapGamma :: (Gamma.T -> Gamma.T) -> T -> T
mapGamma f t = t { gamma = f $ gamma t }

addVariable :: Text -> Types.T -> T -> T
addVariable name typ = mapGamma (Gamma.insert name typ)

getType :: T -> Text -> Maybe Types.T
getType _ name = -- FIXME: using hardcoded list of builtin types for now
  case name of
    "Int" -> pure $ Types.int full
    "Any" -> pure full
    "Empty" -> pure empty
    _ -> Nothing
