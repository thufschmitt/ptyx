module NixLight.Annotations.Parser
  ( typ
  , typeAnnot
  ) where

import qualified NixLight.Annotations as Annot
import qualified Text.Trifecta as Tf
import           Text.Trifecta.Delta (Delta)
import qualified Text.Parser.Token.Style as TStyle
import qualified Text.Parser.Token as Tok
import           Text.Parser.Combinators (skipMany)
import qualified Data.Text as T

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

baseType :: (Tf.TokenParsing m, Monad m) => m Annot.T
baseType = do
  Tok.whiteSpace
  Annot.Ident <$> Tok.ident TStyle.emptyIdents

typ :: (Tf.TokenParsing m, Monad m) => m Annot.T
typ = baseType

typeAnnot :: Delta -> T.Text -> Tf.Result Annot.T
typeAnnot delta = Tf.parseString typ delta . T.unpack
