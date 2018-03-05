{-# LANGUAGE NamedFieldPuns #-}

module Typer.Error (T(..)) where

import Data.Text (Text, unpack)
import Nix.Expr (SrcSpan)

data T = T { location :: SrcSpan, message :: Text }

instance Show T where
  show T{ location, message } =
    show location ++ ": " ++ unpack message
