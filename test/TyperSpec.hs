module TyperSpec (spec) where

import Data.Default
import Test.Hspec
import qualified Nix.Parser as NParser
import qualified NixLight.FromHNix
import qualified Typer.Infer as Infer
import qualified Typer.Error
import qualified Types.Singletons as Singleton
import Types.SetTheoretic
import qualified Types
import qualified Types.Bdd as Bdd
import qualified Types.Arrow as Arrow

shouldSuccessAs :: (Eq a, Show a) => NParser.Result ([Typer.Error.T], a) -> a -> Expectation
shouldSuccessAs (NParser.Success ([], x)) y = x `shouldBe` y
shouldSuccessAs (NParser.Failure f) _ = expectationFailure (show f)
shouldSuccessAs (NParser.Success (errs, _)) _ =
  expectationFailure (show errs)

typeString :: String -> NParser.Result ([Typer.Error.T], Types.T)
typeString s =
  (Infer.expr def . NixLight.FromHNix.expr) <$>
    NParser.parseNixStringLoc s

spec :: Spec
spec =
  describe "Typing tests" $ do
  it "Test integer constant" $
    typeString "1" `shouldSuccessAs` Singleton.int 1
  it "Test trivial lambda" $
    typeString "x: 1" `shouldSuccessAs`
      Types.arrow (Arrow.T $ Bdd.atom $ Arrow.Arrow full (Singleton.int 1))
  it "Test simple lambda" $
    typeString "x /*: Int */: 1" `shouldSuccessAs`
      Types.arrow (Arrow.T $ Bdd.atom $ Arrow.Arrow full (Singleton.int 1))
