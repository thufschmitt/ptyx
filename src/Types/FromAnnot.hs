{-# LANGUAGE OverloadedStrings #-}

module Types.FromAnnot where

import qualified NixLight.Ast as Ast
import qualified NixLight.WithLoc as WL
import qualified Typer.Environ as Env
import qualified Typer.Error as Error
import qualified Types
import qualified Types.Arrow as Arrow
import qualified Types.Bdd as Bdd
import           Types.SetTheoretic
import qualified Types.Singletons as Singleton

import qualified Control.Monad.Writer as W

parse :: Env.T ->  Ast.AnnotLoc -> W.Writer [Error.T] Types.T
parse env annot = case WL.descr annot of
  Ast.Aident name -> case Env.getType env name of
    Nothing -> W.writer (Types.undef, [Error.T (WL.loc annot) "Undefined type"])
    Just t -> pure t
  Ast.Aarrow rawDomain rawCodomain -> do
    domain <- parse env rawDomain
    codomain <- parse env rawCodomain
    pure $ Types.arrow $ Arrow.T $ Bdd.atom $ Arrow.Arrow domain codomain
  Ast.Aor ann1 ann2 -> boolComb cup ann1 ann2
  Ast.Aand ann1 ann2 -> boolComb cap ann1 ann2
  Ast.Adiff ann1 ann2 -> boolComb diff ann1 ann2
  Ast.Aconstant c -> pure $ constant c
  where
    boolComb op ann1 ann2 = do
      t1 <- parse env ann1
      t2 <- parse env ann2
      pure $ op t1 t2

constant :: Ast.Constant -> Types.T
constant (Ast.Cint i) = Singleton.int i
constant (Ast.Cbool b) = Singleton.bool b
