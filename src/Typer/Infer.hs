{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Typer.Infer where


import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified NixLight.Ast as NL
import qualified NixLight.WithLoc as WL
import qualified Typer.Environ as Env
import qualified Typer.Error as Error
import qualified Types
import qualified Types.Arrow as Arrow
import qualified Types.Bdd as Bdd
import qualified Types.Node as Node
import qualified Types.Singletons as S

import qualified Control.Monad.Writer as W

import           Types.SetTheoretic

type WithError = W.Writer [Error.T]

inferExpr :: Env.T -> NL.ExprLoc -> WithError Types.Node
inferExpr env (WL.T loc descr) =
  case descr of
      (NL.Econstant c) -> pure $ inferConstant c
      (NL.Eabs pat body) -> do
        (new_env, domain) <- updateEnv loc env Nothing pat
        codomain <- inferExpr new_env body
        pure . Node.noId .
          Types.arrow .
            Arrow.T . Bdd.atom $ Arrow.Arrow domain codomain
      (NL.Eapp fun arg) -> do
        funType <- inferExpr env fun
        argType <- inferExpr env arg
        checkSubtype loc funType $ (Node.noId $ Types.arrow full)
        let funTypeArrow = Arrow.get $ Types.arrows (Node.typ funType)
        checkSubtype loc argType $ Arrow.compDomain funTypeArrow
        let codom = Node.run mempty $
              Arrow.getApplication
                funTypeArrow
                argType
        pure codom
      (NL.Evar v) ->
        case Env.lookupVariable v env of
          Just t -> pure t
          Nothing -> W.writer
            (Node.noId Types.undef, [Error.T loc "Undefined variable"])
      (NL.Eannot annotType e) -> do
        subExprType <- inferExpr env e
        checkSubtype loc subExprType (Node.noId annotType)
        pure $ Node.noId annotType
      NL.EBinding binds body -> do
        updatedEnv <- bindings env binds
        inferExpr updatedEnv body
      NL.EIfThenElse { NL.eif, NL.ethen, NL.eelse } -> do
        inferIfThenElse env loc eif ethen eelse

inferConstant :: NL.Constant -> Types.Node
inferConstant = Node.noId . \case
  NL.Cint i -> S.int i
  NL.Cbool b -> S.bool b

checkExpr :: Env.T -> Types.Node -> NL.ExprLoc -> WithError ()
checkExpr env expected (WL.T loc descr) =
  case descr of
      NL.Econstant c ->
        checkSubtype loc (inferConstant c) expected
      NL.Evar v ->
        case Env.lookupVariable v env of
          Just t -> checkSubtype loc t expected
          Nothing -> W.tell [Error.T loc "Undefined variable"]
      NL.Eannot annotType e -> do
        checkSubtype loc (Node.noId annotType) expected
        checkExpr env (Node.noId annotType) e
      NL.Eabs pat body -> do
        checkSubtype loc expected $ (Node.noId $ Types.arrow full)
        let arrows = Arrow.decompose . Arrow.get . Types.arrows $ Node.typ expected
        mapM_
          (\(Arrow.Arrow dom codom) -> do
            (newEnv, _) <- updateEnv loc env (Just dom) pat
            checkExpr newEnv codom body)
          arrows
      NL.Eapp fun arg -> do
        argType <- inferExpr env arg
        checkExpr env (Node.noId . Types.arrow $ Arrow.atom argType expected) fun
      NL.EBinding binds body -> do
        updatedEnv <- bindings env binds
        checkExpr updatedEnv expected body
      NL.EIfThenElse { NL.eif, NL.ethen, NL.eelse } -> do
        -- TODO add special case for typecase
        ifType <- inferExpr env eif
        checkSubtype loc ifType (Node.noId $ Types.bool full)
        if ifType <: Node.noId (S.bool False)
          then pure ()
          else checkExpr env expected ethen
        if ifType <: Node.noId (S.bool True)
          then pure ()
          else checkExpr env expected eelse

bindings :: Env.T -> NL.Bindings -> WithError Env.T
bindings externalEnv binds =
  let initialEnv = getInitialEnv (pure externalEnv) binds in
  getFinalEnv initialEnv binds

  where
    getInitialEnv :: WithError Env.T -> NL.Bindings -> WithError Env.T
    getInitialEnv =
      Map.foldlWithKey (\accuEnv varName -> \case
        NL.Inherit -> accuEnv
        NL.NamedVar { NL.annot, NL.rhs = _ } -> do
          env <- accuEnv
          let annotType = fromMaybe Types.undef annot
          pure $ Env.addVariable varName (Node.noId annotType) env)
    getFinalEnv initialEnv =
      Map.foldlWithKey (\accuEnv varName -> \case
        NL.Inherit -> accuEnv
        NL.NamedVar { NL.annot, NL.rhs } -> do
          typingEnv <- initialEnv
          env <- accuEnv
          rhsType <-
            case annot of
              Nothing -> inferExpr typingEnv rhs
              Just annotType -> do
                checkExpr typingEnv (Node.noId annotType) rhs
                pure (Node.noId annotType)
          pure $ Env.addVariable varName rhsType env)
      initialEnv

inferIfThenElse :: Env.T
                -> WL.Loc
                -> NL.ExprLoc -- if
                -> NL.ExprLoc -- then
                -> NL.ExprLoc -- else
                -> WithError Types.Node
inferIfThenElse env loc eif ethen eelse =
  case WL.descr eif of
    NL.Eapp ifFunction (arg@WL.T { WL.descr = NL.Evar v }) -> do
      ifFunctionType <- inferExpr env ifFunction
      case getDiscriminer (Node.typ ifFunctionType) of
        Just discr -> do
          vType <- inferExpr env arg
          thenType <- typeWithExfalso v (vType /\ discr) ethen
          elseType <- typeWithExfalso v (vType /\ neg discr) eelse
          pure $ thenType \/ elseType
        Nothing -> defaultType
    _ -> defaultType
  where
    typeWithExfalso var typ e =
      if typ <: empty
      then pure empty
      else inferExpr (Env.addVariable var typ env) e
    defaultType = do
      ifType <- inferExpr env eif
      checkSubtype loc ifType (Node.noId $ Types.bool full)
      tthen <- if ifType <: Node.noId (S.bool False)
                then pure empty
                else inferExpr env ethen
      telse <- if ifType <: Node.noId (S.bool True)
                then pure empty
                else inferExpr env eelse
      pure $ tthen \/ telse
    getDiscriminer typ | typ <: Types.arrow full =
      let arrows = (Arrow.get $ Types.arrows typ) in
      case (fmap (\(x,y) -> (Set.toList x, Set.toList y)) $
                  Set.toList arrows) of
        [([Arrow.Arrow t1 b1, Arrow.Arrow t2 b2], [])] ->
          if b1 ~: Node.noId (S.bool True)
            && b2 ~: Node.noId (S.bool False)
            && t1 ~: (neg t2)
          then Just t1
          else if b1 ~: Node.noId (S.bool False)
               && b2 ~: Node.noId (S.bool True)
               && t1 ~: neg t2
          then Just t2
          else Nothing
        _ -> Nothing
    getDiscriminer _ = Nothing


updateEnv :: WL.Loc
          -> Env.T
          -> Maybe Types.Node
          -> NL.Pattern
          -> WithError (Env.T, Types.Node)
updateEnv loc env previousAnnot pat = case pat of
  NL.Pvar varName -> do
    let xType = fromMaybe full previousAnnot
    return (Env.addVariable varName xType env, xType)
  NL.Pannot annot sub_pat -> do
    let virtualAnnot = fromMaybe empty previousAnnot
    checkSubtype loc virtualAnnot (Node.noId annot)
    updateEnv loc env (Just $ Node.noId annot) sub_pat

checkSubtype :: WL.Loc -> Types.Node -> Types.Node -> WithError ()
checkSubtype loc t1 t2 =
  if t1 <: t2
  then pure ()
  else W.tell [Error.T loc $ "Expected a subtype of "
                               <> T.pack (show $ Node.typ t2)
                               <> " but got " <> T.pack (show $ Node.typ t1)]
