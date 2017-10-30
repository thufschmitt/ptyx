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

import Data.Function ((&))

import qualified Control.Monad.Writer as W

shouldSuccessAs :: (Eq a, Show a)
                => NParser.Result (W.Writer [Typer.Error.T] a)
                -> a
                -> Expectation
shouldSuccessAs (NParser.Failure f) _ = expectationFailure (show f)
shouldSuccessAs (NParser.Success res) y =
  case W.runWriter res of
    (x, []) -> x `shouldBe` y
    (_, errs) -> expectationFailure (show errs)

shouldFail (NParser.Failure f) _ = expectationFailure (show f)
shouldFail (NParser.Success res) y =
  case W.runWriter res of
    (x, []) -> expectationFailure
                 $ "Expected an error, but got type " ++ show x
    (_, errs) -> pure ()

typeString :: String -> NParser.Result (W.Writer [Typer.Error.T] Types.T)
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
  it "Test trivial annotated lambda" $
    typeString "x /*: Int */: 1" `shouldSuccessAs`
      Types.arrow
        (Arrow.T $ Bdd.atom $ Arrow.Arrow (Types.int full) (Singleton.int 1))
  it "Test simple annotated lambda" $
    typeString "x /*: Int */: x" `shouldSuccessAs`
      Types.arrow
        (Arrow.T $ Bdd.atom $ Arrow.Arrow (Types.int full) (Types.int full))
  it "Test application" $
    typeString "(x: 1) 2" `shouldSuccessAs` Singleton.int 1
  it "Test application2" $
    typeString "(x /*: Int */: x) 2" `shouldSuccessAs` Types.int full
  it "Test wrong application" $
    typeString "(x /*: Empty */: x) 1" & shouldFail
