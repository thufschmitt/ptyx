module NixLight.Annotations.Parser
  ( typ
  , typeAnnot
  ) where

import qualified NixLight.Annotations as Annot
import qualified NixLight.WithLoc as WL
import qualified Text.Trifecta as Tf
import           Text.Trifecta ((<?>))
import           Text.Trifecta.Delta (Delta)
import qualified Text.Parser.Token.Style as TStyle
import qualified Text.Parser.Token as Tok
import           Text.Parser.Expression as PExpr
import           Text.Parser.Combinators (skipMany, optional, choice)
import qualified Data.Text as T
import           Nix.Expr (SrcSpan(..))
import           Data.Fix
import           Control.Applicative ((<|>))

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

annotateLocation3 :: Tf.Parser (a -> b -> c) -> Tf.Parser (a -> b -> WL.T c)
annotateLocation3 p = do
  begin <- Tf.position
  res   <- p
  end   <- Tf.position
  let span = SrcSpan begin end
  pure $ \x y -> WL.T span (res x y)

ident :: Tf.Parser T.Text
ident = Tok.ident TStyle.emptyIdents

baseType :: Tf.Parser Annot.T
baseType = do
  Tok.whiteSpace
  annotateLocation $ Annot.Ident <$> ident

typ :: Tf.Parser Annot.T
typ = PExpr.buildExpressionParser ops atom
      <?> "type"

ops :: PExpr.OperatorTable Tf.Parser Annot.T
ops = [
    [ binary "->" (annotateLocation3 $ pure Annot.Arrow) AssocRight ],
    [ binary "&" (annotateLocation3 $ pure Annot.And) AssocRight ],
    [ binary "|" (annotateLocation3 $ pure Annot.Or) AssocRight ],
    [ binary "\\" (annotateLocation3 $ pure Annot.Diff) AssocRight ]
  ]
  where
    binary :: String -> Tf.Parser (Annot.T -> Annot.T -> Annot.T) -> Assoc -> Operator Tf.Parser Annot.T
    binary  name fun = Infix (fun <* Tok.symbol name)
    prefix  name fun = Prefix (fun <* Tok.symbol name)
    postfix name fun = Postfix (fun <* Tok.symbol name)

atom = Tok.parens typ <|> baseType <?> "simple type"

typeAnnot :: Delta -> T.Text -> Tf.Result Annot.T
typeAnnot delta = Tf.parseString (Tf.space *> typ) delta . T.unpack
