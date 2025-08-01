module Main (main) where

import Control.Comonad.Cofree qualified as CF
import Data.Text qualified as T
import Data.Text.IO qualified as T
import PFL.Compile.Linearise qualified as Linearise
import PFL.Compile.Qualify (qualify)
import PFL.Compile.Thread qualified as Thread
import PFL.Expr.In qualified as In
import PFL.Expr.Qualified qualified as Q
import Test.Sexp qualified as Sexp

type InExpr = CF.Cofree In.Expr ()

type QExpr = CF.Cofree (Q.Expr T.Text T.Text) ()

main :: IO ()
main = do
  testQualify
  testThread
  testLinearise

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

testThread :: IO ()
testThread = do
  T.putStrLn "Testing Thread..."
  doTest
    "simplest"
    "(threaded (! mx))"
    "(abs 0x ((global mx) (local 0x)))"
  where
    doTest :: T.Text -> T.Text -> T.Text -> IO ()
    doTest name inptStr expectedStr = do
      T.putStr $ "  Testing " <> name <> "... "
      inpt :: InExpr <- Sexp.parseFail inptStr
      expected :: QExpr <- Sexp.parseFail expectedStr
      let nms =
            Thread.Names
              { nmThreaded = "threaded",
                nmThread = "!",
                nmPair = "Pair"
              }
      let outpt = Thread.thread nms $ Q.anonymise $ qualify inpt
      case Sexp.unify (Sexp.into outpt) (Sexp.into expected) of
        Right () -> T.putStrLn "OK"
        Left err -> do
          T.putStrLn "FAIL"
          T.putStrLn err

testLinearise :: IO ()
testLinearise = do
  T.putStrLn "Testing Linearise..."
  doTest
    "Simplest copy"
    "((local x) (local x))"
    $ T.unlines
      [ "(match ((global copy) (local x))",
        "  ( (just Pair) (x 0x) ((local 0x) (local x)) )",
        ")"
      ]
  doTest
    "Simplest drop"
    "(abs x (global y))"
    $ T.unlines
      [ "(abs x (match ((global drop) (local x))",
        "  ( (just Unit) () (global y) )",
        "))"
      ]
  where
    doTest :: T.Text -> T.Text -> T.Text -> IO ()
    doTest name inptStr expectedStr = do
      T.putStr $ "  Testing " <> name <> "... "
      inpt :: QExpr <- Sexp.parseFail inptStr
      expected :: QExpr <- Sexp.parseFail expectedStr
      let nms =
            Linearise.Names
              { nmCopy = "copy",
                nmDrop = "drop",
                nmPair = "Pair",
                nmUnit = "Unit"
              }
      let outpt = Linearise.linearise nms $ Q.anonymise inpt
      case Sexp.unify (Sexp.into outpt) (Sexp.into expected) of
        Right () -> T.putStrLn "OK"
        Left err -> do
          T.putStrLn "FAIL"
          T.putStrLn err
