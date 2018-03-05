module TyperSpec (spec) where

import           Data.Default
import qualified Nix.Parser as NParser
import qualified NixLight.Ast as Ast
import qualified NixLight.FromHNix
import           Test.Hspec
import qualified Typer.Error
import qualified Typer.Infer as Infer
import qualified Types
import qualified Types.Arrow as Arrow
import           Types.SetTheoretic
import qualified Types.Singletons as Singleton

import           Data.Function ((&))

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

isInferredAs :: String -> Types.T -> Expectation
isInferredAs prog typ =
  let ast = parseString prog in
  (Infer.inferExpr def <$> ast) `shouldSuccessAs` typ

checksAgain :: String -> Types.T -> Expectation
checksAgain prog typ =
  let ast = parseString prog in
  (Infer.checkExpr def typ <$> ast) `shouldSuccessAs` ()

inferredAndChecks :: String -> Types.T -> Expectation
inferredAndChecks prog typ = do
  isInferredAs prog typ
  checksAgain prog typ

shouldFail :: Show a
           => NParser.Result (W.Writer [Typer.Error.T] a)
           -> b
           -> Expectation
shouldFail (NParser.Failure f) _ = expectationFailure (show f)
shouldFail (NParser.Success res) _y =
  case W.runWriter res of
    (x, []) -> expectationFailure
                 $ "Expected an error, but got type " ++ show x
    (_, _) -> pure ()

typeString :: String -> NParser.Result (W.Writer [Typer.Error.T] Types.T)
typeString s = Infer.inferExpr def <$> parseString s

checkString :: String -> Types.T -> NParser.Result (W.Writer [Typer.Error.T] ())
checkString s typ = Infer.checkExpr def typ <$> parseString s

parseString :: String -> NParser.Result Ast.ExprLoc
parseString s = NixLight.FromHNix.expr <$> NParser.parseNixStringLoc s

spec :: Spec
spec = do
  describe "Inference and check tests" $ do
    it "Integer constant" $
      "1" `inferredAndChecks` Singleton.int 1
    it "Annotated constant" $
      "2 /*: Int */" `inferredAndChecks` Types.int full
    it "Singleton int annot" $
      "2 /*: 2 */" `inferredAndChecks` Singleton.int 2
    it "Singleton bool annot" $
      "true /*: true */" `inferredAndChecks` Singleton.bool True
    it "True constant" $
      "true" `inferredAndChecks` Singleton.bool True
    it "False constant" $
      "false" `inferredAndChecks` Singleton.bool False
    describe "Lambdas" $ do
      it "trivial" $
        "x: 1" `inferredAndChecks`
          Types.arrow (Arrow.atom full (Singleton.int 1))
      it "trivial annotated" $
        "x /*: Int */: 1" `inferredAndChecks`
          Types.arrow (Arrow.atom (Types.int full) (Singleton.int 1))
      it "simple annotated" $
        "x /*: Int */: x" `inferredAndChecks`
          Types.arrow (Arrow.atom (Types.int full) (Types.int full))
      it "higher order" $
        let intarrint =
              Types.arrow (Arrow.atom (Types.int full) (Types.int full))
        in
        "(x /*: Int -> Int */: x)" `inferredAndChecks`
          Types.arrow (Arrow.atom intarrint intarrint)
    describe "Application" $ do
      it "trivial" $
        "(x: 1) 2" `inferredAndChecks` Singleton.int 1
      it "simple" $
        "(x /*: Int */: x) 2" `inferredAndChecks` Types.int full
      it "higher order" $
        "(x /*: Int -> Int */: x 1) (x /*: Int */: x)"
          `inferredAndChecks` Types.int full
      it "intersection1" $
        "let f /*: (Int -> Int) & (Bool -> Bool) */ = x: x; in f 1"
        `inferredAndChecks`
        Types.int full
      it "intersection2" $
        "let f /*: (Int -> Int) & (Bool -> Bool) */ = x: x; in f true"
        `inferredAndChecks`
        Types.bool full
    describe "let-bindings" $ do
      it "trivial" $
        "let x = 1; in x" `inferredAndChecks` Singleton.int 1
      it "trivial annotated" $
        "let x /*: Int */ = 1; in x" `inferredAndChecks` Types.int full
      it "multiple" $
        "let x = 1; y = x; in y" `inferredAndChecks` full
      it "multiple annotated" $
        "let x /*: Int */ = 1; y = x; in y" `inferredAndChecks` Types.int full
    describe "If-then-else" $ do
      it "Always true" $
        "if true then 1 else 2" `inferredAndChecks` Singleton.int 1
      it "Always false" $
        "if false then 1 else 2" `inferredAndChecks` Singleton.int 2
      it "Undecided" $
        "let x /*: Bool */ = true; in if x then 1 else 3"
          `inferredAndChecks` (Singleton.int 1 \/ Singleton.int 3)
    it "wrong" $ do
      typeString "(x /*: Empty */: x) 1" & shouldFail
      checkString "(x /*: Empty */: x) 1" (Types.int full) & shouldFail
    it "undef type" $
      "undefined" `inferredAndChecks` empty
    it "type-annot" $
      "1 /*: Int */" `inferredAndChecks` Types.int full

  describe "Check only" $
    describe "Application" $
      it "identity" $
        "(x: x) 1" `checksAgain` Types.int full
