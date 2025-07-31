module Main (main) where

import Control.Comonad.Cofree qualified as CF
import Data.Text qualified as T
import Data.Text.IO qualified as T
import PFL.Compile.Qualify (qualify)
import PFL.Expr.In qualified as In
import PFL.Expr.Qualified qualified as Q
import Test.Sexp qualified as Sexp

type InExpr = CF.Cofree In.Expr ()

type QExpr = CF.Cofree (Q.Expr T.Text T.Text) ()

main :: IO ()
main = do
  testQualify

testQualify :: IO ()
testQualify = do
  -- TODO many more!
  T.putStrLn "Testing Qualify..."
  doTest
    "id"
    "(abs x x)"
    "(abs x (local x))"
  doTest
    "const global"
    "(abs x y)"
    "(abs x (global y))"
  doTest
    "simple app"
    "(f x)"
    "((global f) (global x))"
  where
    doTest :: T.Text -> T.Text -> T.Text -> IO ()
    doTest name inptStr expectedStr = do
      T.putStr $ "  Testing " <> name <> "... "
      inpt :: InExpr <- Sexp.parseFail inptStr
      expected :: QExpr <- Sexp.parseFail expectedStr
      let outpt = qualify inpt
      case Sexp.unify (Sexp.into outpt) (Sexp.into expected) of
        Right () -> T.putStrLn "OK"
        Left err -> do
          T.putStrLn "FAIL"
          T.putStrLn err
