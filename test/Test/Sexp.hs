module Test.Sexp
  ( Sexp (..),
    Into (..),
    From (..),
    parseFail,
    parse,
    unify,
  )
where

import Control.Category ((>>>))
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree (tailF)
import Control.Monad (zipWithM_, (>=>))
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Functor.Foldable (cata)
import Data.List qualified as L
import Data.Map qualified as M
import Data.String (IsString, fromString)
import Data.Text qualified as T
import PFL.Expr.LambdaLifted qualified as L
import PFL.Expr.Qualified qualified as Q

data Sexp
  = Atom T.Text
  | Lst [Sexp]
  deriving (Eq, Ord)

instance Show Sexp where
  show = \case
    Atom t -> T.unpack t
    Lst ss -> "(" <> L.unwords (show <$> ss) <> ")"

instance IsString Sexp where
  fromString = Atom . T.pack

parsePart :: T.Text -> Either T.Text (Sexp, T.Text)
parsePart =
  T.stripStart >>> T.uncons >>> \case
    Nothing -> Left "EOS in Sexp part"
    Just (c, s)
      | c == '(' -> first ("When parsing list: " <>) $ first Lst <$> goList s
      | otherwise ->
          let (cs, s') = T.break atomChar s
           in Right (Atom (T.cons c cs), s')
  where
    atomChar c = isSpace c || elem c ("()" :: String)

    goList :: T.Text -> Either T.Text ([Sexp], T.Text)
    goList =
      T.stripStart >>> T.uncons >>> \case
        Nothing -> Left "EOS in list"
        Just (')', s) -> Right ([], s)
        Just (c, s) -> do
          (e, s') <- parsePart (T.cons c s)
          first (e :) <$> goList s'

parse :: T.Text -> Either T.Text Sexp
parse =
  parsePart >=> \case
    (e, s) | T.all isSpace s -> pure e
    (_, s) -> Left $ "Unexpected: " <> s

parseFail :: (MonadFail m, From a) => T.Text -> m a
parseFail = either (fail . T.unpack) pure . (parse >=> from)

showText :: (Show a) => a -> T.Text
showText = T.pack . show

unify :: Sexp -> Sexp -> Either T.Text ()
unify = curry $ \case
  (Atom x, Atom y) | x == y -> pure ()
  (Lst xs, Lst ys) -> do
    let merr
          | length xs /= length ys =
              Left $
                T.unwords
                  [ "args length",
                    showText (length xs),
                    "/=",
                    showText (length ys)
                  ]
          | otherwise = zipWithM_ unify xs ys
    first
      ( \err ->
          T.unlines
            [ err,
              "in",
              T.unwords [showText (Lst xs), "/=", showText (Lst ys)]
            ]
      )
      merr
  (s1, s2) -> Left $ T.unwords [showText s1, "/=", showText s2]

class Into a where
  into :: a -> Sexp

instance Into T.Text where
  into = Atom

instance (Into a) => Into [a] where
  into = Lst . fmap into

instance (Into a) => Into (Maybe a) where
  into = \case
    Nothing -> "nothing"
    Just x -> Lst ["just", into x]

instance (Into g, Into l) => Into (CF.Cofree (Q.Expr g l) ann) where
  into =
    cata $
      tailF >>> \case
        Q.Local x -> Lst ["local", into x]
        Q.Global x -> Lst ["global", into x]
        Q.Abs x e -> Lst ["abs", into x, e]
        Q.Ap e1 e2 -> Lst ["ap", e1, e2]
        Q.Match e bs -> Lst ("match" : e : M.foldMapWithKey goBranch bs)
    where
      goBranch :: Maybe g -> ([l], Sexp) -> [Sexp]
      goBranch mc (xs, e) = [Lst [into mc, into xs, e]]

instance (Into g, Into l) => Into (CF.Cofree (L.Expr g l) ann) where
  into =
    cata $
      tailF >>> \case
        L.Local x -> Lst ["local", into x]
        L.Global x -> Lst ["global", into x]
        L.Closure ctx x e -> Lst ["closure", ctx, into x, e]
        L.Ap e1 e2 -> Lst ["ap", e1, e2]
        L.Match e bs -> Lst ("match" : e : M.foldMapWithKey goBranch bs)
    where
      goBranch :: Maybe g -> ([l], Sexp) -> [Sexp]
      goBranch mc (xs, e) = [Lst [into mc, into xs, e]]

class From a where
  from :: Sexp -> Either T.Text a

instance From T.Text where
  from = \case
    Atom s -> pure s
    e -> Left $ "Expected text: " <> showText e

instance (From a) => From (Maybe a) where
  from = \case
    "nothing" -> pure Nothing
    Lst ["just", e] -> Just <$> from e
    e -> Left $ "Expected Maybe: " <> showText e

instance (From a) => From [a] where
  from = \case
    Lst xs -> traverse from xs
    e -> Left $ "Expected list: " <> showText e

instance (Ord g, From g, From l) => From (CF.Cofree (Q.Expr g l) ()) where
  from = \case
    Lst ["local", e] -> (() CF.:<) . Q.Local <$> from e
    Lst ["global", e] -> (() CF.:<) . Q.Global <$> from e
    Lst ["abs", e1, e2] -> (() CF.:<) <$> (Q.Abs <$> from e1 <*> from e2)
    Lst ["ap", e1, e2] -> (() CF.:<) <$> (Q.Ap <$> from e1 <*> from e2)
    Lst ["match", e, Lst bs] -> do
      e' <- from e
      bs' <- M.fromList <$> traverse goBranch bs
      pure $ () CF.:< Q.Match e' bs'
    e -> Left $ "Expected QExpr: " <> showText e
    where
      goBranch ::
        Sexp ->
        Either T.Text (Maybe g, ([l], CF.Cofree (Q.Expr g l) ()))
      goBranch = \case
        Lst [c, xs, e] -> do
          c' <- from c
          xs' <- from xs
          e' <- from e
          pure (c', (xs', e'))
        e -> Left $ "Expected match branch: " <> showText e

instance (Ord g, From g, From l) => From (CF.Cofree (L.Expr g l) ()) where
  from = \case
    Lst ["local", e] -> (() CF.:<) . L.Local <$> from e
    Lst ["global", e] -> (() CF.:<) . L.Global <$> from e
    Lst ["closure", e1, e2, e3] ->
      (() CF.:<)
        <$> (L.Closure <$> from e1 <*> from e2 <*> from e3)
    Lst ["ap", e1, e2] -> (() CF.:<) <$> (L.Ap <$> from e1 <*> from e2)
    Lst ["match", e, Lst bs] -> do
      e' <- from e
      bs' <- M.fromList <$> traverse goBranch bs
      pure $ () CF.:< L.Match e' bs'
    e -> Left $ "Expected LExpr: " <> showText e
    where
      goBranch ::
        Sexp ->
        Either T.Text (Maybe g, ([l], CF.Cofree (L.Expr g l) ()))
      goBranch = \case
        Lst [c, xs, e] -> do
          c' <- from c
          xs' <- from xs
          e' <- from e
          pure (c', (xs', e'))
        e -> Left $ "Expected match branch: " <> showText e
