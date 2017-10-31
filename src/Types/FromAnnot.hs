{-# LANGUAGE OverloadedStrings #-}

module Types.FromAnnot where

import qualified NixLight.Annotations as Annot
import qualified NixLight.WithLoc as WL
import qualified Typer.Environ as Env
import qualified Typer.Error as Error
import qualified Types
import qualified Types.Bdd as Bdd
import qualified Types.Arrow as Arrow

import qualified Control.Monad.Writer as W

parse :: Env.T ->  Annot.T -> W.Writer [Error.T] Types.T
parse env annot = case WL.descr annot of
  Annot.Ident name -> case Env.getType env $ WL.descr name of
    Nothing -> W.writer (Types.undef, [Error.T (WL.loc name) "Undefined type"])
    Just t -> pure t
  Annot.Arrow domain codomain -> do
    domain <- parse env domain
    codomain <- parse env codomain
    pure $ Types.arrow $ Arrow.T $ Bdd.atom $ Arrow.Arrow domain codomain
