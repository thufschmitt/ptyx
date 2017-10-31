{-# LANGUAGE OverloadedStrings #-}

module NixLight.FromHNix where

import Data.Fix (cata, Fix(..))
import Data.Functor.Compose
import qualified NixLight.Ast as NL
import qualified NixLight.WithLoc as WL
import qualified NixLight.Annotations.Parser as AnnotParser
import qualified Text.Trifecta.Delta as TD
import qualified Text.Trifecta as Tri
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
            (NAnnot e ':' annot) ->
              case AnnotParser.typeAnnot (spanBegin loc) annot of
                Tri.Success type_annot ->
                  NL.Eannot type_annot e
                Tri.Failure f -> error $ show f
            _ -> undefined -- TODO
    in
    Compose (WL.T loc descr)

constant :: NAtom -> NL.Constant
constant (NInt i) = NL.Cint i
constant _ = undefined -- TODO

pat :: Params NL.ExprLoc -> NL.Pattern
pat (Param var) = NL.Pvar var
pat (ParamAnnot p ':' annot) =
  case AnnotParser.typeAnnot (TD.Directed "<annot>" 0 0 0 0) annot of
    Tri.Success type_annot ->
      NL.Pannot type_annot (pat p)
    Tri.Failure f -> error $ show f
pat _ = undefined -- TODO
