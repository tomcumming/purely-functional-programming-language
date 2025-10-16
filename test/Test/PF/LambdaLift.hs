module Test.PF.LambdaLift (tests) where

import Control.Category ((>>>))
import Data.Fix (Fix)
import Data.Function ((&))
import Data.Text qualified as T
import PF.Compile.LambdaLift (lambdaLift)
import PF.Expr.Qualified qualified as Q
import Test.Sexp qualified as Sexp
import Test.Sexp.Expr (cf2fix, fix2cf)
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

type QExpr = Q.Expr T.Text

tests :: TestTree
tests =
  testGroup
    "Lambda Lift"
    [ testId,
      testSimple1,
      testSimple2,
      testSimple3
    ]

testExpected :: TestName -> T.Text -> T.Text -> TestTree
testExpected name txtIn txtOut = testCase name $ do
  eIn :: Fix QExpr <- Sexp.fromFail =<< Sexp.parse txtIn
  seOut <- Sexp.parse txtOut
  let eRes = lambdaLift (fix2cf eIn)
  let seRes = Sexp.into (cf2fix eRes)
  Sexp.unify seOut seRes & either (T.unpack >>> fail) pure

testId :: TestTree
testId =
  testExpected
    "id"
    "(lambda i0)"
    "(closure () i0)"

testSimple1 :: TestTree
testSimple1 =
  testExpected
    "\\x y -> y"
    "(lambda (lambda i0))"
    "(closure () (closure () i0))"

testSimple2 :: TestTree
testSimple2 =
  testExpected
    "const"
    "(lambda (lambda i1))"
    "(closure () (closure (i0) i1))"

testSimple3 :: TestTree
testSimple3 =
  testExpected
    "\\x y z-> x y"
    "(lambda (lambda (lambda ((i2 i1) i0))))"
    "(closure () (closure (i0) (closure (i0 i1) ((i2 i1) i0))))"
