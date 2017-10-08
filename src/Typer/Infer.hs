module Typer.Infer where

import Data.Fix (cata)
import Data.Functor.Compose

import qualified NixLight.Ast as NL
import qualified NixLight.WithLoc as WL
import qualified Typer.Environ as Env
import qualified Types
import qualified Types.Singletons as S

expr :: Env.T -> NL.ExprLoc -> Types.T
expr _env = cata phi where
  phi (Compose (WL.T _loc descr)) =
    case descr of
        (NL.Econstant c) -> constant c
        _ -> undefined

constant :: NL.Constant -> Types.T
constant (NL.Cint i) = S.int i
