module NixLight.Annotations
  ( T(..)
  ) where

import Data.Text (Text)

data T
  = Ident !Text
  | Arrow !T !T
  deriving (Ord, Eq, Show)
