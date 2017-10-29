module NixLight.Annotations
  ( T(..)
  ) where

import Data.Text (Text)

data T
  = Ident !Text
  deriving (Ord, Eq, Show)
