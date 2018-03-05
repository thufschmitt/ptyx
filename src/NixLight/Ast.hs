-- | A simplified version of Nix Ast
module NixLight.Ast where

import           Data.Map.Strict (Map)
import           Data.Text (Text)
import qualified NixLight.WithLoc as WL

data NoLocExpr
  = Econstant !Constant
  | Evar !Text
  | Eabs !Pattern !ExprLoc
  | Eapp !ExprLoc !ExprLoc
  | Eannot !AnnotLoc !ExprLoc
  | EBinding !Bindings !ExprLoc
  | EIfThenElse { eif, ethen, eelse :: !ExprLoc }
  deriving (Ord, Eq, Show)

type ExprLoc = WL.T NoLocExpr

data Constant
  = Cint Integer
  | Cbool Bool
  -- TODO: complete
  deriving (Ord, Eq, Show)

data Pattern
  = Pvar !Text
  | Pannot !AnnotLoc !Pattern
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
      annot :: Maybe AnnotLoc,
      rhs :: ExprLoc
    }
  | Inherit
  deriving (Ord, Eq, Show)

data Annot
  = Aident !Text
  | Aarrow !AnnotLoc !AnnotLoc
  | Aor !AnnotLoc !AnnotLoc
  | Aand !AnnotLoc !AnnotLoc
  | Adiff !AnnotLoc !AnnotLoc
  | Aconstant !Constant
  deriving (Ord, Eq, Show)

type AnnotLoc = WL.T Annot
