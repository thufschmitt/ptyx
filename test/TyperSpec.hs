module TyperSpec (spec) where

import Data.Default
import Test.Hspec
import qualified Nix.Parser as NParser
import qualified NixLight.FromHNix
import qualified Typer.Infer as Infer
import qualified Types.Singletons as Singleton

shouldSuccessAs :: (Eq a, Show a) => NParser.Result a -> a -> Expectation
shouldSuccessAs (NParser.Success x) y = x `shouldBe` y
shouldSuccessAs (NParser.Failure f) _ = expectationFailure (show f)

spec :: Spec
spec =
  describe "Typing tests" $
  it "Test integer constant" $ do
    let cst_ast =
          (Infer.expr def . NixLight.FromHNix.expr) <$>
          NParser.parseNixStringLoc "1"
    cst_ast `shouldSuccessAs` Singleton.int 1
