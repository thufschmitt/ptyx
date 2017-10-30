module Types.FromAnnot where

import qualified NixLight.Annotations as Annot
import qualified Typer.Environ as Env
import qualified Types
import qualified Types.Bdd as Bdd
import qualified Types.Arrow as Arrow

parse :: Env.T ->  Annot.T -> Maybe Types.T
parse env annot = case annot of
  Annot.Ident name -> Env.getType env name
  Annot.Arrow domain codomain -> do
    domain <- parse env domain
    codomain <- parse env codomain
    pure $ Types.arrow $ Arrow.T $ Bdd.atom $ Arrow.Arrow domain codomain
