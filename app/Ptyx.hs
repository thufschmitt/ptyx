{-# LANGUAGE LambdaCase #-}
import Nix.Parser
import Nix.Expr

import qualified NixLight.FromHNix
import qualified Typer.Infer as Infer
import qualified Typer.Error as Error
import qualified Types
import Typer.Environ ()

import Data.Default (def)
import System.Environment
import qualified Control.Monad.Writer as W

nix :: FilePath -> IO ()
nix path = parseNixFileLoc path >>= typeAst

nixTypeString :: String -> IO ()
nixTypeString =
  typeAst . parseNixStringLoc

typeAst :: Result NExprLoc -> IO ()
typeAst = \case
  Failure e -> error $ "Parse failed: " ++ show e
  Success n ->
    let nlAst = NixLight.FromHNix.expr n in
    displayTypeResult $ Infer.inferExpr def nlAst

displayTypeResult :: W.Writer [Error.T] Types.T -> IO ()
displayTypeResult res = do
  let (typ, errs) = W.runWriter res
  mapM_ print errs
  print typ

main :: IO ()
main = do
  let usageStr = "Parses a nix file and prints to stdout.\n\
                 \\n\
                 \Usage:\n\
                 \  ptyx --help\n\
                 \  ptyx <path>\n\
                 \  ptyx --expr <expr>\n"
  let argErr msg = error $ "Invalid arguments: " ++ msg ++ "\n" ++ usageStr
  getArgs >>= \case
    "--help":_ -> putStrLn usageStr
    "--expr":expr:_ -> nixTypeString expr
    "--expr":_ -> argErr "Provide an expression."
    ('-':_):_ -> argErr "Provide a path to a nix file."
    path:_ -> nix path
    _ -> argErr "Provide a path to a nix file."
