{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

-- | A simplified version of Nix Ast
module NixLight.Ast where

import Data.Fix
import Data.Text (Text)
import Text.Show.Deriving
import qualified NixLight.WithLoc as WL
import qualified NixLight.Annotations as Annot

data ExprF r
  = Econstant !Constant
  | Evar !Text
  | Eabs !Pattern !r
  | Eapp !r !r
  deriving (Ord, Eq, Functor, Show)

type ExprLocF = WL.TF ExprF

type Expr = Fix ExprF
type ExprLoc = Fix ExprLocF

newtype Constant
  = Cint Integer
  -- TODO: complete
  deriving (Ord, Eq, Show)

data Pattern
  = Pvar !Text
  | Pannot !Annot.T !Pattern
  deriving (Ord, Eq, Show)

$(deriveShow1 ''ExprF)
