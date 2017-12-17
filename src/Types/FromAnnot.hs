{-# LANGUAGE OverloadedStrings #-}

module Types.FromAnnot where

import qualified NixLight.Annotations as Annot
import qualified NixLight.WithLoc as WL
import qualified Typer.Environ as Env
import qualified Typer.Error as Error
import qualified Types
import qualified Types.Bdd as Bdd
import qualified Types.Arrow as Arrow
import           Types.SetTheoretic

import qualified Control.Monad.Writer as W

parse :: Env.T ->  Annot.T -> W.Writer [Error.T] Types.T
parse env annot = case WL.descr annot of
  Annot.Ident name -> case Env.getType env $ name of
    Nothing -> W.writer (Types.undef, [Error.T (WL.loc annot) "Undefined type"])
    Just t -> pure t
  Annot.Arrow domain codomain -> do
    domain <- parse env domain
    codomain <- parse env codomain
    pure $ Types.arrow $ Arrow.T $ Bdd.atom $ Arrow.Arrow domain codomain
  Annot.Or ann1 ann2 -> boolComb cup ann1 ann2
  Annot.And ann1 ann2 -> boolComb cap ann1 ann2
  Annot.Diff ann1 ann2 -> boolComb diff ann1 ann2
  where
    boolComb op ann1 ann2 = do
      t1 <- parse env ann1
      t2 <- parse env ann2
      pure $ op t1 t2
