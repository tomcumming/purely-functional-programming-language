module Main (main) where

import Control.Category ((>>>))
import qualified Control.Comonad.Cofree as CF
import Control.Monad (unless)
import Control.Monad.State (evalState)
import Data.Bifunctor (bimap)
import qualified Data.Map as M
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
      (LL.Closure c1 x1 b1, LL.Closure c2 x2 b2) -> do
        unless (x1 == x2) $ report x1 x2
        unify c1 c2
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
          (Just (x1, e1'), Just (x2, e2')) | x1 == x2 -> unify e1' e2'
          _ -> report d1 d2
      (e1, e2) -> report e1 e2
  where
    report e1 e2 = Left $ T.intercalate " /= " $ T.pack . show <$> [e1, e2]

testLambdaLifting :: IO ()
testLambdaLifting = do
  T.putStrLn "Testing lambda lifting..."
  testLiftingWithoutClosure
  testLiftingClose1
  where
    testLiftingWithoutClosure :: IO ()
    testLiftingWithoutClosure = do
      T.putStr "  Testing lifting without closure... "
      let inExpr :: CF.Cofree Q.Expr () =
            na $
              Q.Abs "x" $
                na $
                  Q.Local "x"
      let outExpr = evalState (lambdaLiftExpr inExpr) (toEnum 0)

      let unit = na $ LL.Global "Unit"
      let (x0, x1) = (LL.Anon $ toEnum 0, LL.Anon $ toEnum 1)
      let expected =
            na $
              LL.Closure
                unit
                x0
                ( na $
                    LL.Match
                      (na $ LL.Local x0)
                      ( M.singleton
                          "Pair"
                          ( [LL.Named "x", x1],
                            na $
                              LL.Match
                                (na $ LL.Local x1)
                                (M.singleton "Unit" ([], na $ LL.Local $ LL.Named "x"))
                                Nothing
                          )
                      )
                      Nothing
                )
      case unify outExpr expected of
        Right () -> T.putStrLn "OK"
        Left err -> do
          T.putStrLn "FAIL"
          T.putStrLn err

    testLiftingClose1 :: IO ()
    testLiftingClose1 = do
      T.putStr "  Testing lifting with closure of single... "
      let inExpr :: CF.Cofree Q.Expr () =
            na $
              Q.Abs "x" $
                na $
                  Q.Abs "y" $
                    na $
                      Q.Local "x"
      let outExpr = evalState (lambdaLiftExpr inExpr) (toEnum 0)

      let unit = na $ LL.Global "Unit"
      let [x0, x1, x2] = LL.Anon . toEnum <$> [0, 1, 2]
      let expected =
            na $
              LL.Closure
                unit
                x0
                ( na $
                    LL.Match
                      (na $ LL.Local x0)
                      ( M.singleton
                          "Pair"
                          ( [LL.Named "x", x1],
                            na $
                              LL.Match
                                (na $ LL.Local x1)
                                ( M.singleton
                                    "Unit"
                                    ( [],
                                      na
                                        $ LL.Closure
                                          (na $ LL.Local $ LL.Named "x")
                                          x2
                                        $ na
                                        $ LL.Match
                                          (na $ LL.Local x2)
                                          ( M.singleton
                                              "Pair"
                                              ( [LL.Named "y", LL.Named "x"],
                                                na $ LL.Local $ LL.Named "x"
                                              )
                                          )
                                          Nothing
                                    )
                                )
                                Nothing
                          )
                      )
                      Nothing
                )

      case unify outExpr expected of
        Right () -> T.putStrLn "OK"
        Left err -> do
          T.putStrLn "FAIL"
          T.putStrLn err
