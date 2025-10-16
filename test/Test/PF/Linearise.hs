module Test.PF.Linearise (tests) where

import Control.Category ((>>>))
import Data.Fix (Fix)
import Data.Function ((&))
import Data.Text qualified as T
import PF.Compile.Linearise (getLinearExpr, linearise)
import PF.Compile.Linearise qualified as Linearise
import PF.Expr.Qualified qualified as Q
import Test.Sexp qualified as Sexp
import Test.Sexp.Expr (cf2fix, fix2cf)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

type QExpr = Q.Expr T.Text

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
      testSimpleDropCopy,
      testCopyAndFrees
    ]

testExpected :: TestName -> T.Text -> T.Text -> TestTree
testExpected name txtIn txtOut = testCase name $ do
  eIn :: Fix QExpr <- Sexp.fromFail =<< Sexp.parse txtIn
  seOut <- Sexp.parse txtOut
  let eRes = getLinearExpr $ linearise names (fix2cf eIn)
  let seRes = Sexp.into (cf2fix eRes)
  Sexp.unify seOut seRes & either (T.unpack >>> fail) pure

testSimpleCopy :: TestTree
testSimpleCopy =
  testExpected
    "Simple Copy"
    "(lambda (i0 i0))"
    "(lambda (match (copy i0) (Tuple 2 (i0 i1))))"

testSimpleDrop :: TestTree
testSimpleDrop =
  testExpected
    "Simple Drop"
    "(lambda hi)"
    "(lambda (match (drop i0) (Unit 0 hi)))"

testSimpleDropCopy :: TestTree
testSimpleDropCopy =
  testExpected
    "Simple Drop & Copy"
    "(lambda (lambda (i1 i1)))"
    "(lambda (lambda (match (drop i0) (Unit 0 (match (copy i1) (Tuple 2 (i0 i1)))))))"

testCopyAndFrees :: TestTree
testCopyAndFrees =
  testExpected
    "\\x y -> (i1 i0) i0"
    "(lambda (lambda ((i1 i0) i0)))"
    "(lambda (lambda (match (copy i0) (Tuple 2 ((i3 i0) i1)))))"
