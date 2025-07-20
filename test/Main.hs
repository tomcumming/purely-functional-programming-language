module Main (main) where

import Control.Category ((>>>))
import qualified Control.Comonad.Cofree as CF
import Control.Monad (unless, zipWithM_)
import Data.Bifunctor (bimap)
import qualified Data.Map.Merge.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import PFL.Compile.LambdaLift (lambdaLiftExpr)
import qualified PFL.Expr.LambdaLifted as LL
import qualified PFL.Expr.Qualified as Q

main :: IO ()
main = testLambdaLifting

-- | No annotation
na :: f (CF.Cofree f ()) -> CF.Cofree f ()
na = (() CF.:<)

unify :: CF.Cofree LL.Expr () -> CF.Cofree LL.Expr () -> Either T.Text ()
unify =
  curry $
    bimap CF.unwrap CF.unwrap >>> \case
      (LL.Local x, LL.Local y) | x == y -> pure ()
      (LL.Global x, LL.Global y) | x == y -> pure ()
      (LL.Closure c1 b1, LL.Closure c2 b2) | length c1 == length c2 -> do
        zipWithM_ unify c1 c2
        unify b1 b2
      (LL.Ap e11 e12, LL.Ap e21 e22) -> do
        unify e11 e21
        unify e12 e22
      (LL.Match e1 bs1 d1, LL.Match e2 bs2 d2) -> do
        unify e1 e2
        _ <-
          M.mergeA
            (M.traverseMissing $ \k _ -> Left $ "Missing " <> k)
            (M.traverseMissing $ \k _ -> Left $ "Missing " <> k)
            ( M.zipWithAMatched $ \_k (xs1, e1') (xs2, e2') -> do
                unless (xs1 == xs2) $ report xs1 xs2
                unify e1' e2'
            )
            bs1
            bs2
        case (d1, d2) of
          (Nothing, Nothing) -> pure ()
          (Just e1', Just e2') -> unify e1' e2'
          _ -> report d1 d2
      (e1, e2) -> report e1 e2
  where
    report e1 e2 = Left $ T.intercalate " /= " $ T.pack . show <$> [e1, e2]

testLambdaLifting :: IO ()
testLambdaLifting = do
  T.putStrLn "Testing lambda lifting..."

  testExpected
    "\\a -> a"
    (na $ Q.Abs $ na $ Q.Local (toEnum 0))
    (na $ LL.Closure [] $ lcl 0)

  testExpected
    "\\a b -> a"
    (na $ Q.Abs $ na $ Q.Abs $ na $ Q.Local (toEnum 1))
    ( na $
        LL.Closure [] $
          na $
            LL.Closure [lcl 0] $
              lcl 1
    )

  testExpected
    "\\a b -> b"
    (na $ Q.Abs $ na $ Q.Abs $ na $ Q.Local (toEnum 0))
    (na $ LL.Closure [] $ na $ LL.Closure [] $ lcl 0)

  testExpected
    "\\a b c -> a"
    (na $ Q.Abs $ na $ Q.Abs $ na $ Q.Abs $ na $ Q.Local (toEnum 2))
    ( na $
        LL.Closure [] $
          na $
            LL.Closure [lcl 0] $
              na $
                LL.Closure [lcl 1] $
                  lcl 1
    )

  testExpected
    "\\a b -> a b"
    ( na $
        Q.Abs $
          na $
            Q.Abs $
              na $
                Q.Ap
                  (na $ Q.Local (toEnum 1))
                  (na $ Q.Local (toEnum 0))
    )
    ( na $
        LL.Closure [] $
          na $
            LL.Closure [lcl 0] $
              na $
                LL.Ap
                  (lcl 1)
                  (lcl 0)
    )

  testExpected
    "\\a b c -> a b"
    ( na $
        Q.Abs $
          na $
            Q.Abs $
              na $
                Q.Abs $
                  na $
                    Q.Ap
                      (na $ Q.Local (toEnum 2))
                      (na $ Q.Local (toEnum 1))
    )
    ( na $
        LL.Closure [] $
          na $
            LL.Closure [lcl 0] $
              na $
                LL.Closure [lcl 1, lcl 0] $
                  na $
                    LL.Ap
                      (lcl 2)
                      (lcl 1)
    )
  where
    testExpected name inExpr expected = do
      T.putStr $ "  Testing " <> name <> "... "
      let outExpr = lambdaLiftExpr inExpr
      case unify outExpr expected of
        Right () -> T.putStrLn "OK"
        Left err -> do
          T.putStrLn "FAIL"
          T.putStrLn err

    lcl = na . LL.Local . toEnum
