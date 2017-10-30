module NixLight.Annotations.Parser
  ( typ
  , typeAnnot
  ) where

import qualified NixLight.Annotations as Annot
import qualified Text.Trifecta as Tf
import           Text.Trifecta.Delta (Delta)
import qualified Text.Parser.Token.Style as TStyle
import qualified Text.Parser.Token as Tok
import           Text.Parser.Combinators (skipMany, optional, choice)
import qualified Data.Text as T

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

baseType :: (Tf.TokenParsing m, Monad m) => m Annot.T
baseType = do
  Tok.whiteSpace
  Annot.Ident <$> Tok.ident TStyle.emptyIdents

arrow :: (Tf.TokenParsing m, Monad m) => m Annot.T
arrow = do
  domain <- atomType
  maybeCodo <- optional $ do
    _ <- Tok.symbol "->"
    typ
  pure $ case maybeCodo of
    Just codomain -> Annot.Arrow domain codomain
    Nothing -> domain

atomType :: (Tf.TokenParsing m, Monad m) => m Annot.T
atomType = choice [ Tok.parens typ, baseType ]

typ :: (Tf.TokenParsing m, Monad m) => m Annot.T
typ = arrow

typeAnnot :: Delta -> T.Text -> Tf.Result Annot.T
typeAnnot delta = Tf.parseString typ delta . T.unpack
