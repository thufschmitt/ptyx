{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NixLight.FromHNix where

import           Control.Monad (Monad, join)
import qualified Control.Monad.State as S
import qualified Control.Monad.Writer as W
import           Data.Default (def)
import           Data.Fix (cataM)
import           Data.Functor.Compose
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Nix.Atoms
import           Nix.Expr
import qualified NixLight.Annotations.Parser as AnnotParser
import qualified NixLight.Ast as NL
import qualified NixLight.WithLoc as WL
import qualified Text.Trifecta as Tri
import qualified Text.Trifecta.Delta as TD
import qualified Typer.Environ.TypeMap as Env
import qualified Typer.Error as Error
import qualified Types
import qualified Types.FromAnnot as FromAnnot

type ConvertConstraint m =
  (W.MonadWriter [Error.T] m, S.MonadState Env.T m, W.MonadFix m)

closedExpr
  :: forall m.
     (W.MonadWriter [Error.T] m, W.MonadFix m)
  => NExprLoc
  -> m NL.ExprLoc
closedExpr ex = fst <$> S.runStateT (expr ex) def

expr
  :: forall m.
     ConvertConstraint m
  => NExprLoc
  -> m NL.ExprLoc
expr = cataM phi where
  phi :: NExprLocF NL.ExprLoc -> m NL.ExprLoc
  phi (Compose (Ann loc e)) =
    let descr =
          case e of
            (NConstant c) -> pure $ NL.Econstant (constant c)
            (NAbs param body) -> pure $ NL.Eabs (pat param) body
            (NApp e1 e2) -> pure $ NL.Eapp e1 e2
            (NSym x) -> pure $ NL.Evar x
            (NAnnot e' (Annotation ':' annot)) ->
              trifectaToWarnings (WL.descr e') $
                flip NL.Eannot e'
                  <$> AnnotParser.typeAnnot (spanBegin loc) annot
            NLet binds e' ->
              pure $ NL.EBinding (bindings binds) e'
            NIf e1 e2 e3 -> pure $ NL.EIfThenElse e1 e2 e3
            _ -> undefined -- TODO
    in
    WL.T loc <$> descr

constant :: NAtom -> NL.Constant
constant (NInt i) = NL.Cint i
constant (NBool b) = NL.Cbool b
constant _ = undefined -- TODO

pat :: Params NL.ExprLoc -> NL.Pattern
pat (Param var) = NL.Pvar var
pat (ParamAnnot p (Annotation ':' annot)) =
  let type_annot = parseTypeAnnot (TD.Directed "<annot>" 0 0 0 0) annot in
  NL.Pannot type_annot (pat p)
pat (ParamAnnot p _) = pat p
pat _ = undefined -- TODO

parseTypeAnnot :: TD.Delta -> T.Text -> NL.AnnotLoc
parseTypeAnnot loc annot =
  case AnnotParser.typeAnnot loc annot of
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

    addQualifiedInherit _baseExpr _accu = \case
      DynamicKey _ -> error "Dynamic keys are not allowed"
      StaticKey _name -> error "Field access not implemented yet"

    addNamedVar [StaticKey name] annot rhs =
      addIfAbsent name (NL.NamedVar annot rhs)
    addNamedVar _ _ _ = error "Not implemented or forbidden lhs"

    addIfAbsent :: T.Text -> NL.BindingDef -> NL.Bindings -> NL.Bindings
    addIfAbsent =
      Map.insertWithKey
        (\kName _ _ -> error $ "Duplicate binding: " ++ T.unpack kName)

trifectaToError :: Tri.ErrInfo -> Error.T
trifectaToError (Tri.ErrInfo messageDoc deltas) =
  let location = case deltas of
        [] -> SrcSpan mempty mempty
        [d0] -> SrcSpan d0 d0
        d1:d2:_ -> SrcSpan d1 d2
      message = T.pack $ show messageDoc
  in
  Error.T { Error.location, Error.message }

-- | Extract a value (with possibly some warnings) out of a 'Tri.Result a'
--
-- If the input is a 'Tri.Failure', then return the default given value with a
-- warning.
trifectaToWarnings
  :: W.MonadWriter [Error.T] m
  => a -- ^ Default value in case of failure
  -> Tri.Result a
  -> m a
trifectaToWarnings defValue = \case
  Tri.Success x -> pure x
  Tri.Failure err -> do
    W.tell [trifectaToError err]
    pure defValue
