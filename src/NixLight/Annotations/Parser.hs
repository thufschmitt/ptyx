module NixLight.Annotations.Parser
  ( typ
  , typeAnnot
  ) where

import qualified NixLight.Annotations as Annot
import qualified NixLight.WithLoc as WL
import qualified Text.Trifecta as Tf
import           Text.Trifecta.Delta (Delta)
import qualified Text.Parser.Token.Style as TStyle
import qualified Text.Parser.Token as Tok
import           Text.Parser.Combinators (skipMany, optional, choice)
import qualified Data.Text as T
import           Nix.Expr (SrcSpan(..))
import           Data.Fix

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

-- Stolen form hnix as it is unexported there
annotateLocation :: Tf.Parser a -> Tf.Parser (WL.T a)
annotateLocation p = do
  begin <- Tf.position
  res   <- p
  end   <- Tf.position
  let span = SrcSpan begin end
  pure $ WL.T span res

ident :: Tf.Parser (WL.T T.Text)
ident = annotateLocation $ Tok.ident TStyle.emptyIdents

baseType :: Tf.Parser Annot.T
baseType = do
  Tok.whiteSpace
  annotateLocation $ Annot.Ident <$> ident

arrow :: Tf.Parser Annot.T
arrow = do
  domain <- atomType
  maybeCodo <- optional $ do
    _ <- Tok.symbol "->"
    typ
  case maybeCodo of
    Just codomain -> annotateLocation $ pure $ Annot.Arrow domain codomain
    Nothing -> pure domain

atomType :: Tf.Parser Annot.T
atomType = choice [ Tok.parens typ, baseType ]

typ :: Tf.Parser Annot.T
typ = arrow

typeAnnot :: Delta -> T.Text -> Tf.Result Annot.T
typeAnnot delta = Tf.parseString typ delta . T.unpack
