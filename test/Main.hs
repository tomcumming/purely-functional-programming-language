module Main (main) where

import Control.Category ((>>>))
import Control.Comonad.Cofree qualified as CF
import Control.Monad (unless, void)
import Data.Bifunctor (bimap)
-- import PFL.Compile.LambdaLift (lambdaLiftExpr)

import Data.Map qualified as M
import Data.Map.Merge.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import PFL.Compile.Linearise (Unique (..), linearise, unLinearised)
import PFL.Expr.Qualified qualified as Q

type QExpr = CF.Cofree (Q.Expr T.Text) ()

type LExpr = CF.Cofree (Q.Expr Unique) ()

main :: IO ()
main = do
  testLinearise

-- | No annotation
na :: f (CF.Cofree f ()) -> CF.Cofree f ()
na = (() CF.:<)

showText :: (Show a) => a -> T.Text
showText = T.pack . show

unify ::
  (Eq l, Show l) =>
  CF.Cofree (Q.Expr l) () ->
  CF.Cofree (Q.Expr l) () ->
  Either T.Text ()
unify =
  curry $
    bimap CF.unwrap CF.unwrap >>> \case
      (Q.Local x, Q.Local y) | x == y -> pure ()
      (Q.Global x, Q.Global y) | x == y -> pure ()
      (Q.Abs x1 b1, Q.Abs x2 b2) -> do
        unless (x1 == x2) $ report x1 x2
        unify b1 b2
      (Q.Ap e11 e12, Q.Ap e21 e22) -> do
        unify e11 e21
        unify e12 e22
      (Q.Match e1 bs1, Q.Match e2 bs2) -> do
        unify e1 e2
        void $
          M.mergeA
            (M.traverseMissing $ \k _ -> Left $ "Missing " <> showText k)
            (M.traverseMissing $ \k _ -> Left $ "Missing " <> showText k)
            ( M.zipWithAMatched $ \_k (xs1, e1') (xs2, e2') -> do
                unless (xs1 == xs2) $ report xs1 xs2
                unify e1' e2'
            )
            bs1
            bs2
      (e1, e2) -> report e1 e2
  where
    report e1 e2 = Left $ T.intercalate " /= " $ showText <$> [e1, e2]

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
      T.putStr $ "Testing " <> name <> "... "
      let outpt = unLinearised $ linearise inpt
      case unify outpt expected of
        Right () -> T.putStrLn "OK"
        Left err -> do
          T.putStrLn "FAIL"
          T.putStrLn err

    lcl x u = na $ Q.Local $ Unique x (toEnum u)
    unit = na $ Q.Global "unit"
    callDrop e = na $ Q.Ap (na $ Q.Global "drop") e
    callCopy e = na $ Q.Ap (na $ Q.Global "copy") e
