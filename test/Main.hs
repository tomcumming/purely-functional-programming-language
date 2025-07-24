module Main (main) where

import Control.Comonad.Cofree qualified as CF
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import PFL.Compile.LambdaLift (lambdaLift)
import PFL.Compile.Linearise (Unique (..), linearise, unLinearised)
import PFL.Expr.LambdaLifted qualified as L
import PFL.Expr.Qualified qualified as Q
import Test.Sexp qualified as Sexp

type QExpr = CF.Cofree (Q.Expr T.Text) ()

type LExpr = CF.Cofree (Q.Expr Unique) ()

type LLExpr = CF.Cofree (L.Expr Unique) ()

main :: IO ()
main = do
  testLinearise
  testLambdaLift

-- | No annotation
na :: f (CF.Cofree f ()) -> CF.Cofree f ()
na = (() CF.:<)

testLinearise :: IO ()
testLinearise = do
  -- TODO many more!
  T.putStrLn "Testing linearise..."
  doTest
    "dropSimple"
    (na $ Q.Abs "x" unit)
    ( na $
        Q.Abs (Unique "x" (toEnum 0)) $
          na $
            Q.Match
              (callDrop $ lcl "x" 0)
              (M.singleton (Just "Unit") ([], unit))
    )

  doTest
    "copySimple"
    (na $ Q.Abs "x" $ na $ Q.Ap (na $ Q.Local "x") (na $ Q.Local "x"))
    ( na $
        Q.Abs (Unique "x" (toEnum 0)) $
          na $
            Q.Match
              (callCopy $ lcl "x" 0)
              ( M.singleton
                  (Just "Pair")
                  ( [Unique "x" (toEnum 0), Unique "x" (toEnum 1)],
                    na $ Q.Ap (lcl "x" 0) (lcl "x" 1)
                  )
              )
    )
  where
    doTest :: T.Text -> QExpr -> LExpr -> IO ()
    doTest name inpt expected = do
      T.putStr $ "  Testing " <> name <> "... "
      let outpt = unLinearised $ linearise inpt
      case Sexp.unify (Sexp.into outpt) (Sexp.into expected) of
        Right () -> T.putStrLn "OK"
        Left err -> do
          T.putStrLn "FAIL"
          T.putStrLn err

    lcl x u = na $ Q.Local $ Unique x (toEnum u)
    unit = na $ Q.Global "unit"
    callDrop e = na $ Q.Ap (na $ Q.Global "drop") e
    callCopy e = na $ Q.Ap (na $ Q.Global "copy") e

testLambdaLift :: IO ()
testLambdaLift = do
  T.putStrLn "Testing lambdaLift..."
  doTest
    "id"
    (na $ Q.Abs "x" $ na $ Q.Local "x")
    ( na $
        L.Closure (na $ L.Global "Unit") (Unique "x" (toEnum 0)) $
          na $
            L.Match (lcl "x" 0) $
              M.singleton
                (Just "Pair")
                ( [Unique "x" (toEnum 0), Unique "ctx" (toEnum 1)],
                  na $
                    L.Match (lcl "ctx" 1) $
                      M.singleton
                        (Just "Unit")
                        ( [],
                          lcl "x" 0
                        )
                )
    )
  where
    doTest :: T.Text -> QExpr -> LLExpr -> IO ()
    doTest name inpt expected = do
      T.putStr $ "  Testing " <> name <> "... "
      let outpt = lambdaLift $ linearise inpt
      case Sexp.unify (Sexp.into outpt) (Sexp.into expected) of
        Right () -> T.putStrLn "OK"
        Left err -> do
          T.putStrLn "FAIL"
          T.putStrLn err

    lcl x u = na $ L.Local $ Unique x (toEnum u)
