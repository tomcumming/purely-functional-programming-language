module Test.PF.Linearise (tests) where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Text qualified as T
import PF.Compile.Linearise qualified as Linearise
import Test.Sexp qualified as Sexp
import Test.Sexp.Expr (cf2fix, fix2cf)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

names :: Linearise.Names T.Text
names =
  Linearise.Names
    { nameUnit = "Unit",
      nameCopy = "copy",
      nameTuple = "Tuple",
      nameDrop = "drop"
    }

tests :: TestTree
tests =
  testGroup
    "Linearise"
    [ testSimpleCopy,
      testSimpleDrop,
      testSimpleDropCopy
    ]

testExpected :: TestName -> Sexp.Sexp -> Sexp.Sexp -> TestTree
testExpected name seIn seOut = testCase name $ do
  eIn <- Sexp.fromFail seIn
  let eRes = Linearise.getLinearExpr $ Linearise.linearise names (fix2cf eIn)
  let seRes = Sexp.into (cf2fix eRes)
  Sexp.unify seOut seRes & either (T.unpack >>> fail) pure

testSimpleCopy :: TestTree
testSimpleCopy =
  testExpected
    "Simple Copy"
    ["lambda", ["i0", "i0"]]
    ["lambda", ["match", ["copy", "i0"], ["Tuple", "2", ["i0", "i1"]]]]

testSimpleDrop :: TestTree
testSimpleDrop =
  testExpected
    "Simple Drop"
    ["lambda", "hi"]
    ["lambda", ["match", ["drop", "i0"], ["Unit", "0", "hi"]]]

testSimpleDropCopy :: TestTree
testSimpleDropCopy =
  testExpected
    "Simple Drop & Copy"
    ["lambda", ["lambda", ["i1", "i1"]]]
    [ "lambda",
      [ "lambda",
        [ "match",
          ["drop", "i0"],
          ["Unit", "0", ["match", ["copy", "i1"], ["Tuple", "2", ["i0", "i1"]]]]
        ]
      ]
    ]
