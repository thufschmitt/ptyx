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

parse :: WL.Loc -> Env.T ->  Annot.T -> W.Writer [Error.T] Types.T
parse loc env annot = case annot of
  Annot.Ident name -> case Env.getType env name of
    Nothing -> W.writer (Types.undef, [Error.T loc "Undefined type"])
    Just t -> pure t
  Annot.Arrow domain codomain -> do
    domain <- parse loc env domain
    codomain <- parse loc env codomain
    pure $ Types.arrow $ Arrow.T $ Bdd.atom $ Arrow.Arrow domain codomain
