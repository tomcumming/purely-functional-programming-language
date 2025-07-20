module PFL.Expr.LambdaLifted (Expr (..), Local (..), GenName, free) where

import Control.Category ((>>>))
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree (tailF)
import Data.Foldable (fold)
import Data.Functor.Classes (Show1)
import Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import Data.Functor.Foldable (cata)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Generics (Generic1)

newtype GenName = GenName Int
  deriving newtype (Eq, Ord, Show, Enum)

data Local
  = Named T.Text
  | Anon GenName
  deriving (Eq, Ord, Show)

data Expr a
  = Local Local
  | Global T.Text
  | Closure a Local a
  | Ap a a
  | -- | This also handles a traditional (mono) let
    Match a (M.Map T.Text ([Local], a)) (Maybe (Local, a))
  deriving (Eq, Ord, Show, Functor, Foldable, Generic1)
  deriving (Show1) via FunctorClassesDefault Expr

free :: CF.Cofree Expr ann -> S.Set Local
free =
  cata $
    tailF >>> \case
      Local x -> S.singleton x
      e@Global {} -> fold e
      Closure ys x zs -> ys <> S.delete x zs
      e@Ap {} -> fold e
      Match ys bs db ->
        ys
          <> foldMap (uncurry S.delete) db
          <> foldMap (uncurry goBranch) bs
  where
    goBranch :: [Local] -> S.Set Local -> S.Set Local
    goBranch xs ys = S.difference ys (S.fromList xs)
