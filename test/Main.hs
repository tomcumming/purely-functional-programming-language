module Main (main) where

import Control.Comonad.Cofree qualified as CF
import Data.Text qualified as T
import Data.Text.IO qualified as T
import PFL.Compile.LambdaLift as LambdaLift (Names (..), lambdaLift)
import PFL.Compile.Linearise as Linearise
  ( Names (..),
    linearise,
    unLinearised,
  )
import PFL.Expr.LambdaLifted qualified as L
import PFL.Expr.Qualified qualified as Q
import Test.Sexp qualified as Sexp

type QExpr = CF.Cofree (Q.Expr T.Text T.Text) ()

type LExpr = CF.Cofree (Q.Expr T.Text T.Text) ()

type LLExpr = CF.Cofree (L.Expr T.Text T.Text) ()

main :: IO ()
main = do
  testLinearise
  testLambdaLift

linNames :: Linearise.Names T.Text
linNames =
  Linearise.Names
    { nmUnit = "Unit",
      nmPair = "Pair",
      nmDrop = "drop",
      nmCopy = "copy"
    }

llNames :: LambdaLift.Names T.Text T.Text
llNames =
  LambdaLift.Names
    { nmUnit = "Unit",
      nmPair = "Pair",
      nmClosure = "ctx"
    }

testLinearise :: IO ()
testLinearise = do
  -- TODO many more!
  T.putStrLn "Testing linearise..."
  doTest
    "dropSimple"
    "(abs x (global unit))"
    $ T.unlines
      [ "(abs x (match",
        "  (ap (global drop) (local x))",
        "  ( ( (just Unit) () (global unit) ) )",
        "))"
      ]
  doTest
    "copySimple"
    "(abs x (ap (local x) (local x)))"
    $ T.unlines
      [ "(abs x (match",
        "  (ap (global copy) (local x))",
        "  ( ( (just Pair) (x x0)",
        "    (ap (local x) (local x0))",
        "  ) )",
        "))"
      ]
  where
    doTest :: T.Text -> T.Text -> T.Text -> IO ()
    doTest name inptStr expectedStr = do
      T.putStr $ "  Testing " <> name <> "... "
      inpt :: QExpr <- Sexp.parseFail inptStr
      expected :: LExpr <- Sexp.parseFail expectedStr
      let outpt = unLinearised $ linearise linNames inpt
      case Sexp.unify (Sexp.into outpt) (Sexp.into expected) of
        Right () -> T.putStrLn "OK"
        Left err -> do
          T.putStrLn "FAIL"
          T.putStrLn err

testLambdaLift :: IO ()
testLambdaLift = do
  T.putStrLn "Testing lambdaLift..."
  doTest
    "id"
    "(abs x (local x))"
    $ T.unlines
      [ "(closure (global Unit) x",
        "  (match (local x) (",
        "    ((just Pair) (x ctx)",
        "      (match (local ctx) (",
        "        ((just Unit) () (local x))",
        "      ))",
        "    )",
        "  ))",
        ")"
      ]
  where
    doTest :: T.Text -> T.Text -> T.Text -> IO ()
    doTest name inptStr expectedStr = do
      T.putStr $ "  Testing " <> name <> "... "
      inpt :: QExpr <- Sexp.parseFail inptStr
      expected :: LLExpr <- Sexp.parseFail expectedStr
      let outpt :: LLExpr = lambdaLift llNames $ linearise linNames inpt
      case Sexp.unify (Sexp.into outpt) (Sexp.into expected) of
        Right () -> T.putStrLn "OK"
        Left err -> do
          T.putStrLn "FAIL"
          T.putStrLn err
