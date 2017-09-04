module NixLight.FromHNix where

import Data.Fix (cata, Fix(..))
import Data.Functor.Compose
import qualified NixLight.Ast as NL
import qualified NixLight.WithLoc as WL
import Nix.Expr
import Nix.Atoms

expr :: NExprLoc -> NL.ExprLoc
expr = cata (Fix . phi) where
  phi (Compose (Ann loc e)) =
    let descr =
          case e of
            (NConstant c) -> NL.Econstant (constant c)
            (NAbs param body) -> NL.Eabs (pat param) body
            (NApp e1 e2) -> NL.Eapp e1 e2
            (NSym x) -> NL.Evar x
            _ -> undefined -- TODO
    in
    Compose (WL.T loc descr)

constant :: NAtom -> NL.Constant
constant (NInt i) = NL.Cint i
constant _ = undefined -- TODO

pat :: Params NL.ExprLoc -> NL.Pattern
pat (Param var) = NL.Pvar var
pat _ = undefined -- TODO
