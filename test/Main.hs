module Main (main) where

import Control.Comonad.Cofree qualified as CF
import Data.Text qualified as T
import PFL.Compile.Linearise qualified as Linearise
import PFL.Compile.Qualify (qualify)
import PFL.Compile.Thread qualified as Thread
import PFL.Expr.In qualified as In
import PFL.Expr.Qualified qualified as Q
import Test.Sexp qualified as Sexp
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

type InExpr = CF.Cofree In.Expr ()

type QExpr = CF.Cofree (Q.Expr T.Text T.Text) ()

main :: IO ()
main =
  defaultMain $
    testGroup
      "PFL"
      [ testQualify,
        testThread,
        testLinearise
      ]

testQualify :: TestTree
testQualify =
  testGroup
    "Qualify"
    [ testExpected
        "id"
        "(abs x x)"
        "(abs x (local x))",
      testExpected
        "const global"
        "(abs x y)"
        "(abs x (global y))",
      testExpected
        "simple app"
        "(f x)"
        "((global f) (global x))"
    ]
  where
    testExpected name inptStr expectedStr = testCase name $ do
      inpt :: InExpr <- Sexp.parseFail inptStr
      expected :: QExpr <- Sexp.parseFail expectedStr
      let outpt = qualify inpt
      case Sexp.unify (Sexp.into outpt) (Sexp.into expected) of
        Right () -> pure ()
        Left err -> fail (T.unpack err)

testThread :: TestTree
testThread =
  testGroup
    "Thread"
    [ testExpected
        "simplest"
        "(threaded (! mx))"
        "(abs 0x ((global mx) (local 0x)))"
    ]
  where
    testExpected name inptStr expectedStr = testCase name $ do
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
        Right () -> pure ()
        Left err -> fail (T.unpack err)

testLinearise :: TestTree
testLinearise =
  testGroup
    "Linearise"
    [ testExpected
        "Simplest copy"
        "((local x) (local x))"
        $ T.unlines
          [ "(match ((global copy) (local x))",
            "  ( (just Pair) (x 0x) ((local 0x) (local x)) )",
            ")"
          ],
      testExpected
        "Simplest drop"
        "(abs x (global y))"
        $ T.unlines
          [ "(abs x (match ((global drop) (local x))",
            "  ( (just Unit) () (global y) )",
            "))"
          ]
    ]
  where
    testExpected name inptStr expectedStr = testCase name $ do
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
        Right () -> pure ()
        Left err -> fail (T.unpack err)
