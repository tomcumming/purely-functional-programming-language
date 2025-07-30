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
import PFL.Expr.In qualified as In
import PFL.Expr.Qualified qualified as Q
import Text.Read (readMaybe)

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

instance Into Sexp where
  into = id

instance Into T.Text where
  into = Atom

instance (Into l) => Into (Q.Local l) where
  into = \case
    Q.Named x -> into x
    Q.Anon n -> Atom $ showText n <> "x"

instance (Into a) => Into [a] where
  into = Lst . fmap into

instance (Into a) => Into (Maybe a) where
  into = \case
    Nothing -> "nothing"
    Just x -> Lst ["just", into x]

instance Into (CF.Cofree In.Expr ann) where
  into =
    cata $
      tailF >>> \case
        In.EVar x -> Atom x
        In.Abs x e -> Lst ["abs", into x, into e]
        In.Ap e1 e2 -> Lst [into e1, into e2]
        In.Match e bs -> Lst ("match" : into e : M.foldMapWithKey goBranch bs)
    where
      goBranch :: Maybe T.Text -> ([T.Text], Sexp) -> [Sexp]
      goBranch mc (xs, e) = [Lst [into mc, into xs, e]]

instance (Into g, Into l, Into e) => Into (Q.Val g l e) where
  into = \case
    Q.Local x -> Lst ["local", into x]
    Q.Global x -> Lst ["global", into x]
    Q.Abs x e -> Lst ["abs", into x, into e]

instance (Into g, Into l) => Into (CF.Cofree (Q.Expr g l) ann) where
  into =
    cata $
      tailF >>> \case
        Q.Val v -> into v
        Q.Ap v1 v2 x e ->
          Lst
            ["let", into x, "=", into v1, into v2, "in", into e]
        Q.Match e bs -> Lst ("match" : into e : M.foldMapWithKey goBranch bs)
    where
      goBranch :: Maybe g -> ([l], Sexp) -> [Sexp]
      goBranch mc (xs, e) = [Lst [into mc, into xs, e]]

class From a where
  from :: Sexp -> Either T.Text a

instance From T.Text where
  from = \case
    Atom s -> pure s
    e -> Left $ "Expected text: " <> showText e

instance (From l) => From (Q.Local l) where
  from = \case
    Atom x
      | Just ns <- T.stripSuffix "x" x,
        Just n <- readMaybe (T.unpack ns) ->
          Right $ Q.Anon n
    e | Right l <- from e -> pure $ Q.Named l
    e -> Left $ "Expected Local: " <> showText e

instance (From a) => From (Maybe a) where
  from = \case
    "nothing" -> pure Nothing
    Lst ["just", e] -> Just <$> from e
    e -> Left $ "Expected Maybe: " <> showText e

instance (From a) => From [a] where
  from = \case
    Lst xs -> traverse from xs
    e -> Left $ "Expected list: " <> showText e

instance From (CF.Cofree In.Expr ()) where
  from = \case
    Lst ["abs", x, e] -> (() CF.:<) <$> (In.Abs <$> from x <*> from e)
    Lst ("match" : e : bs) -> do
      e' <- from e
      bs' <- M.fromList <$> traverse goBranch bs
      pure $ () CF.:< In.Match e' bs'
    Lst [e1, e2] -> (() CF.:<) <$> (In.Ap <$> from e1 <*> from e2)
    Atom x -> pure $ () CF.:< In.EVar x
    e -> Left $ "Expected InExpr: " <> showText e
    where
      goBranch ::
        Sexp ->
        Either T.Text (Maybe T.Text, ([T.Text], CF.Cofree In.Expr ()))
      goBranch = \case
        Lst [c, xs, e] -> do
          c' <- from c
          xs' <- from xs
          e' <- from e
          pure (c', (xs', e'))
        e -> Left $ "Expected match branch: " <> showText e

instance (From g, From l, From e) => From (Q.Val g l e) where
  from = \case
    Lst ["local", e] -> Q.Local <$> from e
    Lst ["global", e] -> Q.Global <$> from e
    Lst ["abs", e1, e2] -> Q.Abs <$> from e1 <*> from e2
    e -> Left $ "Unexpected QVal: " <> showText e

instance (Ord g, From g, From l) => From (CF.Cofree (Q.Expr g l) ()) where
  from = \case
    e | Right v <- from e -> pure $ () CF.:< Q.Val v
    Lst ["let", e1, "=", e2, e3, "in", e4] -> do
      x <- from e1
      v1 <- from e2
      v2 <- from e3
      e <- from e4
      pure $ () CF.:< Q.Ap v1 v2 x e
    Lst ("match" : e : bs) -> do
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
