{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.FromAnnot where

import           Prelude hiding (id)

import qualified NixLight.Ast as Ast
import qualified NixLight.WithLoc as WL
import qualified Typer.Environ.TypeMap as Env
import qualified Typer.Error as Error
import qualified Types
import qualified Types.Arrow as Arrow
import qualified Types.Bdd as Bdd
import qualified Types.Node as Node
import           Types.SetTheoretic
import qualified Types.Singletons as Singleton
import qualified Types.UId as UId

import           Control.Monad.Fix (mfix)
import qualified Control.Monad.Writer as W
import           Data.Monoid ((<>))

parse
  :: (W.MonadWriter [Error.T] m, W.MonadFix m, UId.MonadGen m)
  => Env.T
  -> Ast.AnnotLoc
  -> m Types.T
parse env annot = Node.typ <$> parseNode env annot

parseNode
  :: (W.MonadWriter [Error.T] m, W.MonadFix m, UId.MonadGen m)
  => Env.T
  -> Ast.AnnotLoc
  -> m (Node.T Types.T)
parseNode env annot = case WL.descr annot of
  Ast.Aident name -> case Env.lookup name env of
    Nothing -> W.writer (Node.noId $ Types.undef, [Error.T (WL.loc annot) "Undefined type"])
    Just t -> pure t
  Ast.Aarrow rawDomain rawCodomain -> do
    domain <- parseNode env rawDomain
    codomain <- parseNode env rawCodomain
    pure $ Node.noId $ Types.arrow $ Arrow.T $ Bdd.atom $ Arrow.Arrow domain codomain
  Ast.Aor ann1 ann2 -> boolComb cup ann1 ann2
  Ast.Aand ann1 ann2 -> boolComb cap ann1 ann2
  Ast.Adiff ann1 ann2 -> boolComb diff ann1 ann2
  Ast.Aconstant c -> pure $ constant c
  Ast.Awhere binds subAnnot -> do
    newEnv <- bindings env binds
    parseNode newEnv subAnnot
  where
    boolComb op ann1 ann2 = do
      t1 <- parseNode env ann1
      t2 <- parseNode env ann2
      pure $ W.liftM2 op t1 t2

constant :: Ast.Constant -> Types.Node
constant (Ast.Cint i) = Node.noId $ Singleton.int i
constant (Ast.Cbool b) = Node.noId $ Singleton.bool b

bindings
  :: (W.MonadWriter [Error.T] m, W.MonadFix m, UId.MonadGen m)
  => Env.T
  -> Ast.Abindings
  -> m Env.T
bindings initEnv binds =
  let mkFinalEnv finalEnv = Env.T <$> mapM (parseRhs $ initEnv <> finalEnv) binds in
  (<> initEnv) <$> mfix mkFinalEnv
  where
    parseRhs env annot = do
      parsed <- parseNode env annot
      id <- case Node.id parsed of
              Nothing -> UId.fresh
              Just i -> pure i
      pure $ parsed { Node.id = Just id }
