{-# LANGUAGE NamedFieldPuns #-}

module Typer.Error (T(..)) where

import Nix.Expr (SrcSpan)
import Data.Text (Text, unpack)

data T = T { location :: SrcSpan, message :: Text }

instance Show T where
  show T{ location, message } =
    show location ++ ": " ++ unpack message
