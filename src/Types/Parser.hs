module Types.Parser
  ( typ
  , parseTypeAnnot
  ) where

import qualified Types
import           Types.Intervals ()
import           Types.SetTheoretic (full)
import qualified Text.Trifecta as Tf
import           Text.Trifecta.Delta (Delta)
import qualified Text.Parser.Token.Style as TStyle
import qualified Text.Parser.Token as Tok
import qualified Data.Text as T

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

baseTypeNames :: Map String Types.T
baseTypeNames = Map.fromList [
    ("Int", Types.int full)
 ]

baseType :: (Tf.TokenParsing m, Monad m) => m Types.T
baseType = do
  typeName <- Tok.ident TStyle.emptyIdents
  case Map.lookup typeName baseTypeNames of
    Nothing -> fail "Not a basic type"
    Just t -> return t

typ :: (Tf.TokenParsing m, Monad m) => m Types.T
typ = baseType

parseTypeAnnot :: Delta -> T.Text -> Tf.Result Types.T
parseTypeAnnot delta =
  Tf.parseString typ delta . T.unpack
