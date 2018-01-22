module NixLight.Annotations
  ( T
  , Ast.Annot(..)
  ) where

import Data.Text (Text)

import qualified NixLight.WithLoc as WL
import qualified NixLight.Ast as Ast

type T = Ast.AnnotLoc
