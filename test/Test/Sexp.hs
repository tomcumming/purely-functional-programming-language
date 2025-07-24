module Test.Sexp
  ( Sexp (..),
    Into (..),
    unify,
  )
where

import Control.Category ((>>>))
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree (tailF)
import Control.Monad (zipWithM_)
import Data.Bifunctor (first)
import Data.Functor.Foldable (cata)
import Data.List qualified as L
import Data.Map qualified as M
import Data.String (IsString, fromString)
import Data.Text qualified as T
import PFL.Compile.Linearise (Uid (..), Unique (..))
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

instance Into Uid where
  into (Uid u) = Atom $ showText u

instance Into Unique where
  into (Unique x u) = Lst [into x, into u]

instance (Into l) => Into (CF.Cofree (Q.Expr l) ann) where
  into =
    cata $
      tailF >>> \case
        Q.Local x -> Lst ["local", into x]
        Q.Global x -> Lst ["global", into x]
        Q.Abs x e -> Lst [Atom "abs", into x, e]
        Q.Ap e1 e2 -> Lst ["ap", e1, e2]
        Q.Match e bs -> Lst ("match" : e : M.foldMapWithKey goBranch bs)
    where
      goBranch :: Maybe T.Text -> ([l], Sexp) -> [Sexp]
      goBranch mc (xs, e) = [Lst ["branch", into mc, into xs, e]]

instance (Into l) => Into (CF.Cofree (L.Expr l) ann) where
  into =
    cata $
      tailF >>> \case
        L.Local x -> Lst ["local", into x]
        L.Global x -> Lst ["global", into x]
        L.Closure ctx x e -> Lst [Atom "closure", ctx, into x, e]
        L.Ap e1 e2 -> Lst ["ap", e1, e2]
        L.Match e bs -> Lst ("match" : e : M.foldMapWithKey goBranch bs)
    where
      goBranch :: Maybe T.Text -> ([l], Sexp) -> [Sexp]
      goBranch mc (xs, e) = [Lst ["branch", into mc, into xs, e]]
