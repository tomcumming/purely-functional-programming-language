module Test.Sexp
  ( Sexp (..),
    Into (..),
    From (..),
    showSexp,
    fromFail,
    unify,
  )
where

import Control.Category ((>>>))
import Control.Monad (zipWithM_)
import Data.Bifunctor (first)
import Data.List qualified as L
import Data.String (IsString, fromString)
import Data.Text qualified as T
import GHC.IsList (IsList, Item, fromList, toList)

data Sexp
  = Atom T.Text
  | Lst [Sexp]
  deriving (Eq, Ord)

instance Show Sexp where
  show = \case
    Atom t -> T.unpack t
    Lst ss -> "[" <> L.unwords (show <$> ss) <> "]"

instance IsString Sexp where
  fromString = Atom . T.pack

instance IsList Sexp where
  type Item Sexp = Sexp
  fromList = Lst
  toList = \case
    Lst ss -> ss
    Atom {} -> error "Can't toList an Atom"

showSexp :: (Show a) => a -> Sexp
showSexp = T.show >>> Atom

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
    Nothing -> "nothing"
    Just x -> Lst ["just", into x]

class From a where
  from :: Sexp -> Either T.Text a

instance From T.Text where
  from = \case
    Atom s -> pure s
    e -> Left $ "Expected text: " <> T.show e

instance (From a) => From (Maybe a) where
  from = \case
    "nothing" -> pure Nothing
    Lst ["just", e] -> Just <$> from e
    e -> Left $ "Expected Maybe: " <> T.show e

instance (From a) => From [a] where
  from = \case
    Lst xs -> traverse from xs
    e -> Left $ "Expected list: " <> T.show e
