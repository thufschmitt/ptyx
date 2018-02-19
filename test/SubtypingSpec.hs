module SubtypingSpec (spec) where

import Test.Hspec
import qualified Types.Singletons as Singleton
import Types.SetTheoretic
import qualified Types
import qualified Types.Arrow as Arrow

spec :: Spec
spec = do
  describe "Same-kind subtyping" $ do
    describe "integer" $ do
      it "1<:Any" $ Singleton.int 1 <: Types.int full
      it "1/<:2" $ not $ Singleton.int 1 <: Singleton.int 2
    describe "boolean" $ do
      it "true<:Any" $ Singleton.bool True <: Types.bool full
      it "true/<:false" $ not $ Singleton.bool True <: Singleton.bool False
    describe "arrow" $ do
      it "a->b<:Any" $ Arrow.atom (full :: Types.T) empty <: full
      it "codmain-covariant" $
        Arrow.atom (full :: Types.T) empty <: Arrow.atom full full
      it "domain-contravariant" $
        Arrow.atom (full :: Types.T) empty <: Arrow.atom empty empty
  describe "Inter-kind" $ do
    it "Empty<:Any" $ (empty :: Types.T) <: full
    it "Any</:Empty" $ not $ (full :: Types.T) <: empty
    it "bool-int" $ not $ Singleton.int 1 <: Types.bool full
    it "int-bool" $ not $ Singleton.bool True <: Types.int full
    it "int-arrow" $ not $ Singleton.int 1 <: Types.arrow full
