-- | A simplified version of Nix Ast
module NixLight.Ast where

import Data.Text (Text)
import           Data.Map.Strict (Map)
import qualified NixLight.WithLoc as WL
import qualified NixLight.Annotations as Annot

data NoLocExpr
  = Econstant !Constant
  | Evar !Text
  | Eabs !Pattern !ExprLoc
  | Eapp !ExprLoc !ExprLoc
  | Eannot !Annot.T !ExprLoc
  | EBinding !Bindings !ExprLoc
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

type Bindings = Map Text BindingDef

-- | Nix's binding can be of three forms (we currently only consider the case
-- where the lhs is a variable and not a more complex attribute path):
-- 1. x = e; (to which we add an optional type annotation)
-- 2. inherit x1 … xn;
-- 3. inherit (e) x1 _ xn;
--
-- The first one is kept as it is, the second one is translated to n binding of
-- the form @inherit xi@ (or mor exactly @xi = inherit@ and the third one is
-- translated to n binding of the form @xi = r.xi@.
data BindingDef
  = NamedVar {
      annot :: Maybe Annot.T,
      rhs :: ExprLoc
    }
  | Inherit
  deriving (Ord, Eq, Show)
