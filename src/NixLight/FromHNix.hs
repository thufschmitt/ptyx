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
            (NAbs param body) -> flip NL.Eabs body <$> pat param
            (NApp e1 e2) -> pure $ NL.Eapp e1 e2
            (NSym x) -> pure $ NL.Evar x
            (NAnnot e' (Annotation ':' typeAnnot)) ->
              flip NL.Eannot e' <$> annot (spanBegin loc) typeAnnot
            NLet binds e' ->
              flip NL.EBinding e' <$> bindings loc binds
            NIf e1 e2 e3 -> pure $ NL.EIfThenElse e1 e2 e3
            _ -> undefined -- TODO
    in
    WL.T loc <$> descr

constant :: NAtom -> NL.Constant
constant (NInt i) = NL.Cint i
constant (NBool b) = NL.Cbool b
constant _ = undefined -- TODO

pat
  :: ConvertConstraint m
  => Params NL.ExprLoc
  -> m NL.Pattern
pat (Param var) = pure $ NL.Pvar var
pat (ParamAnnot p (Annotation ':' ann)) = do
  typeAnnot <- annot (TD.Directed "<annot>" 0 0 0 0) ann
  subPat <- pat p
  pure $ NL.Pannot typeAnnot subPat
pat (ParamAnnot p _) = pat p
pat _ = undefined -- TODO

parseTypeAnnot :: TD.Delta -> T.Text -> NL.AnnotLoc
parseTypeAnnot loc annot =
  case AnnotParser.typeAnnot loc annot of
    Tri.Success type_annot -> type_annot
    Tri.Failure f -> error $ show f

bindings
  :: forall m.
     ConvertConstraint m
  => SrcSpan
  -> [Binding NL.ExprLoc]
  -> m NL.Bindings
bindings loc =
  foldl (\accu binding ->
    case binding of
      Inherit Nothing names -> foldl (addInherit loc) accu names
      Inherit (Just e) vars  -> foldl (addQualifiedInherit e) accu vars
      NamedVar attrPath ann rhs -> do
        let typeAnnot = ann >>= (\case
              Annotation ':' txt -> Just txt
              _ -> Nothing)
        parsedTypeAnnot <- case typeAnnot of
          Just a -> Just <$> annot (TD.Directed "<annot>" 0 0 0 0) a
          Nothing -> pure Nothing
        addNamedVar loc attrPath parsedTypeAnnot rhs =<< accu
      )
    (pure Map.empty)
  where
    addInherit :: SrcSpan -> m NL.Bindings -> NKeyName NL.ExprLoc -> m NL.Bindings
    addInherit loc accu = \case
      -- FIXME: Actually some trivial dynamic keys such as @"foo"@ are
      -- considered by nix as static, so we should also allow them here
      DynamicKey _ ->
        W.tell [ Error.T loc "Dynamic keys are not allowed" ] *> accu
      StaticKey name ->
        addIfAbsent loc name NL.Inherit =<< accu

    addQualifiedInherit _baseExpr _accu = \case
      DynamicKey _ -> error "Dynamic keys are not allowed"
      StaticKey _name -> error "Field access not implemented yet"

    addNamedVar loc [StaticKey name] annot rhs =
      addIfAbsent loc name (NL.NamedVar annot rhs)
    addNamedVar _ _ _ _ = error "Not implemented or forbidden lhs"

    addIfAbsent :: SrcSpan -> T.Text -> NL.BindingDef -> NL.Bindings -> m NL.Bindings
    addIfAbsent loc key value =
      Map.alterF
        (\case
           Nothing -> pure $ Just value
           Just _ ->
            W.writer
              (Just value, [Error.T loc $"Duplicate binding: " W.<> key])
        )
        key

annot
  :: ConvertConstraint m
  => TD.Delta
  -> T.Text
  -> m Types.T
annot delta text = do
  env <- S.get
  join $
    trifectaToWarnings
        (pure Types.undef)
        (FromAnnot.parse env <$> AnnotParser.typeAnnot delta text)

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
