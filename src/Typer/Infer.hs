{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Typer.Infer where

import Data.Functor.Compose

import Data.Fix (Fix(Fix), cata)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified NixLight.Ast as NL
import qualified NixLight.WithLoc as WL
import qualified NixLight.Annotations as Annot
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

expr :: Env.T -> NL.ExprLoc -> WithError Types.T
expr env (WL.T loc descr) =
  case descr of
      (NL.Econstant c) -> pure $ constant c
      (NL.Eabs pat body) -> do
        (new_env, domain) <- updateEnv loc env Nothing pat
        codomain <- expr new_env body
        pure $
          Types.arrow $
            Arrow.T $ Bdd.atom $ Arrow.Arrow domain codomain
      (NL.Eapp fun arg) -> do
        funType <- expr env fun
        argType <- expr env arg
        checkSubtype loc funType $ Types.arrow full
        let Arrow.Arrow dom codom =
              Arrow.getApplication
                (Types.arrows funType)
                argType
        checkSubtype loc argType dom
        pure codom
      (NL.Evar v) ->
        case Env.lookupVariable v env of
          Just t -> pure t
          Nothing -> W.writer (Types.undef, [Error.T loc "Undefined variable"])
      (NL.Eannot annot e) -> do
        subExprType <- expr env e
        annotType <- Types.FromAnnot.parse env annot
        checkSubtype loc subExprType annotType
        pure annotType
      NL.EBinding binds body -> do
        updatedEnv <- bindings env binds
        expr updatedEnv body

constant :: NL.Constant -> Types.T
constant (NL.Cint i) = S.int i

bindings :: Env.T -> NL.Bindings -> WithError Env.T
bindings env binds =
  let initialEnv = getInitialEnv (pure env) binds in
  getFinalEnv initialEnv binds

  where
    getInitialEnv =
      Map.foldlWithKey (\accuEnv varName -> \case
        NL.Inherit -> accuEnv
        NL.NamedVar { NL.annot, NL.rhs } -> do
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
          accuEnv <- accuEnv
          rhsType <- expr typingEnv rhs
          pure $ Env.addVariable varName rhsType accuEnv)
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
  else W.tell [Error.T loc "Subtyping failure"]
