module Test.Sexp
  ( Sexp (..),
    Into (..),
    From (..),
    parse,
    showAtom,
    fromFail,
    unify,
  )
where

import Control.Category ((>>>))
import Control.Monad (void, zipWithM_)
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Function ((&))
import Data.List qualified as L
import Data.Text qualified as T
import Text.ParserCombinators.ReadP qualified as R
import Text.Read (readEither)

data Sexp
  = Atom T.Text
  | Lst [Sexp]
  deriving (Eq, Ord)

instance Show Sexp where
  show = \case
    Atom t -> T.unpack t
    Lst ss -> "(" <> L.unwords (show <$> ss) <> ")"

instance Read Sexp where
  readsPrec _ = R.readP_to_S sexpReadP

sexpReadP :: R.ReadP Sexp
sexpReadP =
  R.skipSpaces >> R.look >>= \case
    '(' : _ -> do
      void $ R.char '('
      ss <- R.many sexpReadP
      R.skipSpaces
      void $ R.char ')'
      pure $ Lst ss
    _ -> Atom . T.pack <$> R.munch1 atom
  where
    atom = \case
      ')' -> False
      '(' -> False
      c -> isSpace c & not

parse :: (MonadFail m) => T.Text -> m Sexp
parse = T.unpack >>> readEither >>> either fail pure

showAtom :: (Show a) => a -> Sexp
showAtom = T.show >>> Atom

fromFail :: (MonadFail m, From a) => Sexp -> m a
fromFail = either (fail . T.unpack) pure . from

unify :: Sexp -> Sexp -> Either T.Text ()
unify = curry $ \case
  (Atom x, Atom y) | x == y -> pure ()
  (Lst xs, Lst ys) -> do
    let merr
          | length xs /= length ys =
              Left $
                T.unwords
                  [ "args length",
                    T.show (length xs),
                    "/=",
                    T.show (length ys)
                  ]
          | otherwise = zipWithM_ unify xs ys
    first
      ( \err ->
          T.unlines
            [ err,
              "in",
              T.unwords [T.show (Lst xs), "/=", T.show (Lst ys)]
            ]
      )
      merr
  (s1, s2) -> Left $ T.unwords [T.show s1, "/=", T.show s2]

class Into a where
  into :: a -> Sexp

instance Into Sexp where
  into = id

instance Into T.Text where
  into = Atom

instance (Into a) => Into [a] where
  into = Lst . fmap into

instance (Into a) => Into (Maybe a) where
  into = \case
    Nothing -> Atom "nothing"
    Just x -> Lst [Atom "just", into x]

class From a where
  from :: Sexp -> Either T.Text a

instance From T.Text where
  from = \case
    Atom s -> pure s
    e -> Left $ "Expected text: " <> T.show e

instance (From a) => From (Maybe a) where
  from = \case
    Atom "nothing" -> pure Nothing
    Lst [Atom "just", e] -> Just <$> from e
    e -> Left $ "Expected Maybe: " <> T.show e

instance (From a) => From [a] where
  from = \case
    Lst xs -> traverse from xs
    e -> Left $ "Expected list: " <> T.show e
