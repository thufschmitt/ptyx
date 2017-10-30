{-# LANGUAGE NamedFieldPuns #-}

module Typer.Error (T(..)) where

import Text.Trifecta.Delta (Delta)
import Data.Text (Text, unpack)

data T = T { location :: Delta, message :: Text }

instance Show T where
  show T{ location, message } =
    show location ++ ": " ++ unpack message
