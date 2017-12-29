{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Typer.Infer where


import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified NixLight.Ast as NL
import qualified NixLight.WithLoc as WL
import qualified Typer.Environ as Env
import qualified Typer.Error as Error
import qualified Types
import qualified Types.Singletons as S
import qualified Types.FromAnnot
import qualified Types.Bdd as Bdd
import qualified Types.Arrow as Arrow

import qualified Control.Monad.Writer as W

import Types.SetTheoretic

type WithError = W.Writer [Error.T]

inferExpr :: Env.T -> NL.ExprLoc -> WithError Types.T
inferExpr env (WL.T loc descr) =
  case descr of
      (NL.Econstant c) -> pure $ inferConstant c
      (NL.Eabs pat body) -> do
        (new_env, domain) <- updateEnv loc env Nothing pat
        codomain <- inferExpr new_env body
        pure $
          Types.arrow $
            Arrow.T $ Bdd.atom $ Arrow.Arrow domain codomain
      (NL.Eapp fun arg) -> do
        funType <- inferExpr env fun
        argType <- inferExpr env arg
        checkSubtype loc funType $ Types.arrow full
        let funTypeArrow = Arrow.get $ Types.arrows funType
        checkSubtype loc argType $ Arrow.compDomain funTypeArrow
        let codom =
              Arrow.getApplication
                funTypeArrow
                argType
        pure codom
      (NL.Evar v) ->
        case Env.lookupVariable v env of
          Just t -> pure t
          Nothing -> W.writer (Types.undef, [Error.T loc "Undefined variable"])
      (NL.Eannot annot e) -> do
        subExprType <- inferExpr env e
        annotType <- Types.FromAnnot.parse env annot
        checkSubtype loc subExprType annotType
        pure annotType
      NL.EBinding binds body -> do
        updatedEnv <- bindings env binds
        inferExpr updatedEnv body
      NL.EIfThenElse _ _ _ -> undefined

inferConstant :: NL.Constant -> Types.T
inferConstant (NL.Cint i) = S.int i
inferConstant (NL.Cbool b) = S.bool b

checkExpr :: Env.T -> Types.T -> NL.ExprLoc -> WithError ()
checkExpr env expected (WL.T loc descr) =
  case descr of
      NL.Econstant c ->
        checkSubtype loc (inferConstant c) expected
      NL.Evar v ->
        case Env.lookupVariable v env of
          Just t -> checkSubtype loc t expected
          Nothing -> W.tell [Error.T loc "Undefined variable"]
      NL.Eannot annot e -> do
        annotType <- Types.FromAnnot.parse env annot
        checkSubtype loc annotType expected
        checkExpr env annotType e
      NL.Eabs pat body -> do
        checkSubtype loc expected $ Types.arrow full
        let arrows = Arrow.decompose $ Arrow.get $ Types.arrows expected
        mapM_
          (\(Arrow.Arrow dom codom) -> do
            (newEnv, _) <- updateEnv loc env (Just dom) pat
            checkExpr newEnv codom body)
          arrows
      NL.Eapp fun arg -> do
        argType <- inferExpr env arg
        checkExpr env (Types.arrow $ Arrow.atom argType expected) fun
      NL.EBinding binds body -> do
        updatedEnv <- bindings env binds
        checkExpr updatedEnv expected body
      NL.EIfThenElse _ _ _ -> undefined

bindings :: Env.T -> NL.Bindings -> WithError Env.T
bindings externalEnv binds =
  let initialEnv = getInitialEnv (pure externalEnv) binds in
  getFinalEnv initialEnv binds

  where
    getInitialEnv =
      Map.foldlWithKey (\accuEnv varName -> \case
        NL.Inherit -> accuEnv
        NL.NamedVar { NL.annot, NL.rhs = _ } -> do
          env <- accuEnv
          annotType <- case annot of
            Nothing -> pure Types.undef
            Just a -> Types.FromAnnot.parse env a
          pure $ Env.addVariable varName annotType env)
    getFinalEnv initialEnv =
      Map.foldlWithKey (\accuEnv varName -> \case
        NL.Inherit -> accuEnv
        NL.NamedVar { NL.annot, NL.rhs } -> do
          typingEnv <- initialEnv
          env <- accuEnv
          rhsType <-
            case annot of
              Nothing -> inferExpr typingEnv rhs
              Just t -> do
                annotType <- Types.FromAnnot.parse typingEnv t
                checkExpr typingEnv annotType rhs
                pure annotType
          pure $ Env.addVariable varName rhsType env)
      initialEnv


updateEnv :: WL.Loc
          -> Env.T
          -> Maybe Types.T
          -> NL.Pattern
          -> WithError (Env.T, Types.T)
updateEnv loc env previousAnnot pat = case pat of
  NL.Pvar varName -> do
    let xType = fromMaybe full previousAnnot
    return (Env.addVariable varName xType env, xType)
  NL.Pannot annot sub_pat -> do
    annotatedType <- Types.FromAnnot.parse env annot
    let virtualAnnot = fromMaybe full previousAnnot
    updateEnv loc env (Just $ cap annotatedType virtualAnnot) sub_pat

checkSubtype :: WL.Loc -> Types.T -> Types.T -> WithError ()
checkSubtype loc t1 t2 =
  if sub t1 t2
  then pure ()
  else W.tell [Error.T loc $ "Expected a subtype of "
                               <> T.pack (show t2)
                               <> " but got " <> T.pack (show t1)]
