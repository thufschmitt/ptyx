module Typer.Environ where

import qualified Typer.Environ.Gamma as Gamma

newtype T = T { gamma :: Gamma.T }
