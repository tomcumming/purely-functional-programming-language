{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Sexp.Expr (cf2fix, fix2cf) where

import Control.Category ((>>>))
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree (tailF)
import Control.Monad (forM)
import Data.Fix (Fix (Fix))
import Data.Functor.Foldable (cata)
import Data.Map qualified as M
import Data.Text qualified as T
import GHC.IsList qualified as IsList
import PF.Expr.LambdaLifted qualified as L
import PF.Expr.Qualified qualified as Q
import Test.Sexp qualified as Sexp
import Text.Read (readMaybe)

cf2fix :: (Functor f) => CF.Cofree f a -> Fix f
cf2fix = cata (tailF >>> Fix)

fix2cf :: (Functor f) => Fix f -> CF.Cofree f ()
fix2cf = cata (() CF.:<)

instance (Sexp.Into g) => Sexp.Into (Fix (Q.Expr g)) where
  into = cata $ \case
    Q.Local l -> Sexp.Atom $ "i" <> T.show (fromEnum l)
    Q.Global g -> Sexp.into g
    Q.Abs e -> ["lambda", e]
    Q.Ap e1 e2 -> [e1, e2]
    Q.Match e1 bs ->
      IsList.fromList $
        ["match", e1]
          <> M.foldMapWithKey
            ( \g (n, e) ->
                [ Sexp.Lst
                    (maybe [] (Sexp.into >>> pure) g <> [Sexp.showSexp n, e])
                ]
            )
            bs

instance (Sexp.From g, Ord g) => Sexp.From (Fix (Q.Expr g)) where
  from = \case
    Sexp.Atom t
      | Just s <- T.stripPrefix "i" t,
        Just w <- readMaybe (T.unpack s) ->
          Right $ Fix $ Q.Local $ toEnum w
    e | Right g <- Sexp.from e -> Right $ Fix $ Q.Global g
    Sexp.Lst es
      | ["lambda", e] <- es -> Fix . Q.Abs <$> Sexp.from e
      | [e1, e2] <- es -> Fix <$> (Q.Ap <$> Sexp.from e1 <*> Sexp.from e2)
      | ["match", e1, Sexp.Lst bs] <- es -> do
          e1' <- Sexp.from e1
          bs' <- fmap M.fromList . forM bs $ \case
            Sexp.Lst [Sexp.Atom n, e]
              | Just n' <- readMaybe (T.unpack n) ->
                  (Nothing,) . (n',) <$> Sexp.from e
            Sexp.Lst [g, Sexp.Atom n, e]
              | Just n' <- readMaybe (T.unpack n) -> do
                  g' <- Sexp.from g
                  (Just g',) . (n',) <$> Sexp.from e
            b -> Left $ "Not a parsable branch: " <> T.show b
          Right $ Fix $ Q.Match e1' bs'
    e -> Left $ "Not a parsable QExpr: " <> T.show e

instance (Sexp.Into g) => Sexp.Into (Fix (L.Expr g)) where
  into = cata $ \case
    L.Local l -> Sexp.Atom $ "i" <> T.show (fromEnum l)
    L.Global g -> Sexp.into g
    L.Closure ctx e -> ["closure", Sexp.Lst $ Sexp.into <$> ctx, e]
    L.Ap e1 e2 -> [e1, e2]
    L.Match e1 bs ->
      IsList.fromList $
        ["match", e1]
          <> M.foldMapWithKey
            ( \g (n, e) ->
                [ Sexp.Lst
                    (maybe [] (Sexp.into >>> pure) g <> [Sexp.showSexp n, e])
                ]
            )
            bs

instance (Sexp.From g, Ord g) => Sexp.From (Fix (L.Expr g)) where
  from = \case
    Sexp.Atom t
      | Just s <- T.stripPrefix "i" t,
        Just w <- readMaybe (T.unpack s) ->
          Right $ Fix $ L.Local $ toEnum w
    e | Right g <- Sexp.from e -> Right $ Fix $ L.Global g
    Sexp.Lst es
      | ["closure", Sexp.Lst ctx, e] <- es ->
          Fix
            <$> (L.Closure <$> traverse Sexp.from ctx <*> Sexp.from e)
      | [e1, e2] <- es -> Fix <$> (L.Ap <$> Sexp.from e1 <*> Sexp.from e2)
      | ["match", e1, Sexp.Lst bs] <- es -> do
          e1' <- Sexp.from e1
          bs' <- fmap M.fromList . forM bs $ \case
            Sexp.Lst [Sexp.Atom n, e]
              | Just n' <- readMaybe (T.unpack n) ->
                  (Nothing,) . (n',) <$> Sexp.from e
            Sexp.Lst [g, Sexp.Atom n, e]
              | Just n' <- readMaybe (T.unpack n) -> do
                  g' <- Sexp.from g
                  (Just g',) . (n',) <$> Sexp.from e
            b -> Left $ "Not a parsable branch: " <> T.show b
          Right $ Fix $ L.Match e1' bs'
    e -> Left $ "Not a parsable QExpr: " <> T.show e
