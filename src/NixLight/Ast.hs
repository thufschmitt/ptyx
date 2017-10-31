-- | A simplified version of Nix Ast
module NixLight.Ast where

import Data.Fix
import Data.Text (Text)
import Text.Show.Deriving
import qualified NixLight.WithLoc as WL
import qualified NixLight.Annotations as Annot

data NoLocExpr
  = Econstant !Constant
  | Evar !Text
  | Eabs !Pattern !ExprLoc
  | Eapp !ExprLoc !ExprLoc
  | Eannot !Annot.T !ExprLoc
  deriving (Ord, Eq, Show)

type ExprLoc = WL.T NoLocExpr

newtype Constant
  = Cint Integer
  -- TODO: complete
  deriving (Ord, Eq, Show)

data Pattern
  = Pvar !Text
  | Pannot !Annot.T !Pattern
  deriving (Ord, Eq, Show)
