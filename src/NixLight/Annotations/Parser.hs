module NixLight.Annotations.Parser
  ( typ
  , typeAnnot
  ) where

import qualified NixLight.Ast as Ast
import qualified NixLight.WithLoc as WL
import qualified Text.Trifecta as Tf
import           Text.Trifecta ((<?>))
import           Text.Trifecta.Delta (Delta)
import qualified Text.Parser.Token.Style as TStyle
import qualified Text.Parser.Token as Tok
import           Text.Parser.Expression as PExpr
import qualified Data.Text as T
import           Nix.Expr (SrcSpan(..))
import           Control.Applicative ((<|>))

-- Stolen form hnix as it is unexported there
annotateLocation :: Tf.Parser a -> Tf.Parser (WL.T a)
annotateLocation p = do
  begin <- Tf.position
  res   <- p
  end   <- Tf.position
  let srcspan = SrcSpan begin end
  pure $ WL.T srcspan res

annotateLocation3 :: Tf.Parser (a -> b -> c) -> Tf.Parser (a -> b -> WL.T c)
annotateLocation3 p = do
  begin <- Tf.position
  res   <- p
  end   <- Tf.position
  let srcspan = SrcSpan begin end
  pure $ \x y -> WL.T srcspan (res x y)

ident :: Tf.Parser T.Text
ident = Tok.ident TStyle.emptyIdents

baseType :: Tf.Parser Ast.AnnotLoc
baseType = do
  Tok.whiteSpace
  annotateLocation $ Ast.Aident <$> ident

typ :: Tf.Parser Ast.AnnotLoc
typ = PExpr.buildExpressionParser ops atom
      <?> "type"

ops :: PExpr.OperatorTable Tf.Parser Ast.AnnotLoc
ops = [
    [ binary "->" (annotateLocation3 $ pure Ast.Aarrow) AssocRight ],
    [ binary "&" (annotateLocation3 $ pure Ast.Aand) AssocRight ],
    [ binary "|" (annotateLocation3 $ pure Ast.Aor) AssocRight ],
    [ binary "\\" (annotateLocation3 $ pure Ast.Adiff) AssocRight ]
  ]
  where
    binary :: String -> Tf.Parser (Ast.AnnotLoc -> Ast.AnnotLoc -> Ast.AnnotLoc) -> Assoc -> Operator Tf.Parser Ast.AnnotLoc
    binary  name fun = Infix (fun <* Tok.symbol name)
    -- prefix  name fun = Prefix (fun <* Tok.symbol name)
    -- postfix name fun = Postfix (fun <* Tok.symbol name)

atom :: Tf.Parser Ast.AnnotLoc
atom = Tok.parens typ <|> baseType <?> "simple type"

typeAnnot :: Delta -> T.Text -> Tf.Result Ast.AnnotLoc
typeAnnot delta = Tf.parseString (Tf.space *> typ) delta . T.unpack
