module NixLight.Annotations
  ( T, NoAnnotT(..)
  ) where

import Data.Text (Text)

import qualified NixLight.WithLoc as WL


data NoAnnotT
  = Ident !Text
  | Arrow !T !T
  | Or !T !T
  | And !T !T
  | Diff !T !T
  deriving (Ord, Eq, Show)

type T = (WL.T NoAnnotT)
