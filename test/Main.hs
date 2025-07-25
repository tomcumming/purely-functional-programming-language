module Main (main) where

import Control.Comonad.Cofree qualified as CF
import Data.Text qualified as T
import Data.Text.IO qualified as T
import PFL.Compile.LambdaLift (lambdaLift)
import PFL.Compile.Linearise (Unique (..), linearise, unLinearised)
import PFL.Expr.LambdaLifted qualified as L
import PFL.Expr.Qualified qualified as Q
import Test.Sexp qualified as Sexp

type QExpr = CF.Cofree (Q.Expr T.Text T.Text) ()

type LExpr = CF.Cofree (Q.Expr T.Text Unique) ()

type LLExpr = CF.Cofree (L.Expr T.Text Unique) ()

main :: IO ()
main = do
  testLinearise
  testLambdaLift

testLinearise :: IO ()
testLinearise = do
  -- TODO many more!
  T.putStrLn "Testing linearise..."
  doTest
    "dropSimple"
    "(abs x (global unit))"
    $ T.unlines
      [ "(abs (x 0) (match",
        "  (ap (global drop) (local (x 0)))",
        "  ( ( (just Unit) () (global unit) ) )",
        "))"
      ]
  doTest
    "copySimple"
    "(abs x (ap (local x) (local x)))"
    $ T.unlines
      [ "(abs (x 0) (match",
        "  (ap (global copy) (local (x 0)))",
        "  ( ( (just Pair) ((x 0) (x 1))",
        "    (ap (local (x 0)) (local (x 1)))",
        "  ) )",
        "))"
      ]
  where
    doTest :: T.Text -> T.Text -> T.Text -> IO ()
    doTest name inptStr expectedStr = do
      T.putStr $ "  Testing " <> name <> "... "
      inpt :: QExpr <- Sexp.parseFail inptStr
      expected :: LExpr <- Sexp.parseFail expectedStr
      let outpt = unLinearised $ linearise inpt
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
      [ "(closure (global Unit) (x 0)",
        "  (match (local (x 0)) (",
        "    ((just Pair) ((x 0) (ctx 1))",
        "      (match (local (ctx 1)) (",
        "        ((just Unit) () (local (x 0)))",
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
      let outpt = lambdaLift $ linearise inpt
      case Sexp.unify (Sexp.into outpt) (Sexp.into expected) of
        Right () -> T.putStrLn "OK"
        Left err -> do
          T.putStrLn "FAIL"
          T.putStrLn err
