{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module NixLight.FromHNix where

import Data.Fix (cata, Fix(..))
import Data.Functor.Compose
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified NixLight.Ast as NL
import qualified NixLight.WithLoc as WL
import qualified NixLight.Annotations.Parser as AnnotParser
import qualified NixLight.Annotations as Annot
import qualified Text.Trifecta.Delta as TD
import qualified Text.Trifecta as Tri
import Nix.Expr
import Nix.Atoms

expr :: NExprLoc -> NL.ExprLoc
expr = cata phi where
  phi (Compose (Ann loc e)) =
    let descr =
          case e of
            (NConstant c) -> NL.Econstant (constant c)
            (NAbs param body) -> NL.Eabs (pat param) body
            (NApp e1 e2) -> NL.Eapp e1 e2
            (NSym x) -> NL.Evar x
            (NAnnot e (Annotation ':' annot)) ->
              case AnnotParser.typeAnnot (spanBegin loc) annot of
                Tri.Success type_annot ->
                  NL.Eannot type_annot e
                Tri.Failure f -> error $ show f
            NLet binds e ->
              NL.EBinding (bindings binds) e
            _ -> undefined -- TODO
    in
    WL.T loc descr

constant :: NAtom -> NL.Constant
constant (NInt i) = NL.Cint i
constant _ = undefined -- TODO

pat :: Params NL.ExprLoc -> NL.Pattern
pat (Param var) = NL.Pvar var
pat (ParamAnnot p (Annotation ':' annot)) =
  let type_annot = parseTypeAnnot (TD.Directed "<annot>" 0 0 0 0) annot in
  NL.Pannot type_annot (pat p)
pat (ParamAnnot p _) = pat p
pat _ = undefined -- TODO

parseTypeAnnot :: TD.Delta -> T.Text -> Annot.T
parseTypeAnnot loc annot =
  case AnnotParser.typeAnnot (TD.Directed "<annot>" 0 0 0 0) annot of
    Tri.Success type_annot -> type_annot
    Tri.Failure f -> error $ show f

bindings :: [Binding NL.ExprLoc] -> NL.Bindings
bindings =
  foldl (\accu binding ->
    case binding of
      Inherit Nothing names -> foldl addInherit accu names
      Inherit (Just e) vars  -> foldl (addQualifiedInherit e) accu vars
      NamedVar attrPath annot rhs ->
        let typeAnnot = annot >>= (\case
              Annotation ':' txt -> Just txt
              _ -> Nothing)
        in
        let parsedTypeAnnot = parseTypeAnnot (TD.Directed "<annot>" 0 0 0 0) <$> typeAnnot in
        addNamedVar attrPath parsedTypeAnnot rhs accu
      )
    Map.empty
  where
    addInherit :: NL.Bindings -> NKeyName NL.ExprLoc -> NL.Bindings
    addInherit accu = \case
      -- FIXME: Actually some trivial dynamic keys such as @"foo"@ are
      -- considered by nix as static, so we should also allow them here
      DynamicKey _ -> error "Dynamic keys are not allowed"
      StaticKey name -> addIfAbsent name NL.Inherit accu

    addQualifiedInherit baseExpr accu = \case
      DynamicKey _ -> error "Dynamic keys are not allowed"
      StaticKey name -> error "Field access not implemented yet"

    addNamedVar [StaticKey name] annot rhs =
      addIfAbsent name (NL.NamedVar annot rhs)
    addNamedVar _ _ _ = error "Not implemented or forbidden lhs"

    addIfAbsent :: T.Text -> NL.BindingDef -> NL.Bindings -> NL.Bindings
    addIfAbsent =
      Map.insertWithKey
        (\kName _ _ -> error $ "Duplicate binding: " ++ T.unpack kName)
