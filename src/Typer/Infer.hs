module Typer.Infer where

import Data.Fix (cata)
import Data.Functor.Compose

import Data.Fix (Fix(Fix))
import Data.Maybe (fromMaybe)
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

import Types.SetTheoretic

expr :: Env.T -> NL.ExprLoc -> ([Error.T], Types.T)
expr env (Fix (Compose (WL.T _loc descr))) =
  case descr of
      (NL.Econstant c) -> pure $ constant c
      (NL.Eabs pat body) -> do
        (new_env, domain) <- updateEnv env Nothing pat
        codomain <- expr new_env body
        pure $
          Types.arrow $
            Arrow.T $ Bdd.atom $ Arrow.Arrow domain codomain
      (NL.Eapp fun arg) -> do
        funType <- expr env fun
        argType <- expr env arg
        checkSubtype funType $ Types.arrow full
        let Arrow.Arrow dom codom =
              Arrow.getApplication
                (Types.arrows funType)
                argType
        pure codom
      _ -> undefined

constant :: NL.Constant -> Types.T
constant (NL.Cint i) = S.int i

updateEnv :: Env.T -> Maybe Types.T -> NL.Pattern -> ([Error.T], (Env.T, Types.T))
updateEnv env previousAnnot pat = case pat of
  NL.Pvar varName -> do
    let xType = fromMaybe full previousAnnot
    return (Env.addVariable varName xType env, xType)
  NL.Pannot annot sub_pat -> do
    annotatedType <- case Types.FromAnnot.parse env annot of
      Nothing -> error "This should no go through error"
      Just typ -> pure typ
    let virtualAnnot = fromMaybe full previousAnnot
    updateEnv env (Just $ cap annotatedType virtualAnnot) sub_pat

checkSubtype :: Types.T -> Types.T -> ([Error.T], ())
checkSubtype t1 t2 =
  if sub t1 t2
  then pure ()
  else error "This also should not go through error"
