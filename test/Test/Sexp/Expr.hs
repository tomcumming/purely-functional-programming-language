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
import PF.Expr.LambdaLifted qualified as L
import PF.Expr.Qualified qualified as Q
import Test.Sexp (From (..), Into (..), Sexp (..), showAtom)
import Text.Read (readMaybe)

cf2fix :: (Functor f) => CF.Cofree f a -> Fix f
cf2fix = cata (tailF >>> Fix)

fix2cf :: (Functor f) => Fix f -> CF.Cofree f ()
fix2cf = cata (() CF.:<)

instance (Into g) => Into (Fix (Q.Expr g)) where
  into = cata $ \case
    Q.Local l -> Atom $ "i" <> T.show (fromEnum l)
    Q.Global g -> into g
    Q.Abs e -> Lst [Atom "lambda", e]
    Q.Ap e1 e2 -> Lst [e1, e2]
    Q.Match e1 bs ->
      Lst $
        [Atom "match", e1]
          <> M.foldMapWithKey
            ( \g (n, e) ->
                [ Lst
                    (maybe [] (into >>> pure) g <> [showAtom n, e])
                ]
            )
            bs

instance (From g, Ord g) => From (Fix (Q.Expr g)) where
  from = \case
    Atom t
      | Just s <- T.stripPrefix "i" t,
        Just w <- readMaybe (T.unpack s) ->
          Right $ Fix $ Q.Local $ toEnum w
    e | Right g <- from e -> Right $ Fix $ Q.Global g
    Lst es
      | [Atom "lambda", e] <- es -> Fix . Q.Abs <$> from e
      | [e1, e2] <- es -> Fix <$> (Q.Ap <$> from e1 <*> from e2)
      | [Atom "match", e1, Lst bs] <- es -> do
          e1' <- from e1
          bs' <- fmap M.fromList . forM bs $ \case
            Lst [Atom n, e]
              | Just n' <- readMaybe (T.unpack n) ->
                  (Nothing,) . (n',) <$> from e
            Lst [g, Atom n, e]
              | Just n' <- readMaybe (T.unpack n) -> do
                  g' <- from g
                  (Just g',) . (n',) <$> from e
            b -> Left $ "Not a parsable branch: " <> T.show b
          Right $ Fix $ Q.Match e1' bs'
    e -> Left $ "Not a parsable QExpr: " <> T.show e

instance (Into g) => Into (Fix (L.Expr g)) where
  into = cata $ \case
    L.Local l -> Atom $ "i" <> T.show (fromEnum l)
    L.Global g -> into g
    L.Closure ctx e -> Lst [Atom "closure", Lst $ into <$> ctx, e]
    L.Ap e1 e2 -> Lst [e1, e2]
    L.Match e1 bs ->
      Lst $
        [Atom "match", e1]
          <> M.foldMapWithKey
            ( \g (n, e) ->
                [ Lst
                    (maybe [] (into >>> pure) g <> [showAtom n, e])
                ]
            )
            bs

instance (From g, Ord g) => From (Fix (L.Expr g)) where
  from = \case
    Atom t
      | Just s <- T.stripPrefix "i" t,
        Just w <- readMaybe (T.unpack s) ->
          Right $ Fix $ L.Local $ toEnum w
    e | Right g <- from e -> Right $ Fix $ L.Global g
    Lst es
      | [Atom "closure", Lst ctx, e] <- es ->
          Fix
            <$> (L.Closure <$> traverse from ctx <*> from e)
      | [e1, e2] <- es -> Fix <$> (L.Ap <$> from e1 <*> from e2)
      | [Atom "match", e1, Lst bs] <- es -> do
          e1' <- from e1
          bs' <- fmap M.fromList . forM bs $ \case
            Lst [Atom n, e]
              | Just n' <- readMaybe (T.unpack n) ->
                  (Nothing,) . (n',) <$> from e
            Lst [g, Atom n, e]
              | Just n' <- readMaybe (T.unpack n) -> do
                  g' <- from g
                  (Just g',) . (n',) <$> from e
            b -> Left $ "Not a parsable branch: " <> T.show b
          Right $ Fix $ L.Match e1' bs'
    e -> Left $ "Not a parsable QExpr: " <> T.show e
