module Types.FromAnnot where

import qualified NixLight.Annotations as Annot
import qualified Typer.Environ as Env
import qualified Types

parse :: Env.T ->  Annot.T -> Maybe Types.T
parse env annot = case annot of
  Annot.Ident name -> Env.getType env name
