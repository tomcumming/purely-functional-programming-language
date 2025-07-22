module PFL.Expr.LambdaLifted (Expr (..), free) where

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

data Expr l a
  = Local l
  | Global T.Text
  | Closure a l a
  | Ap a a
  | -- | This also handles a traditional (mono) let
    Match a (M.Map T.Text ([l], a)) (Maybe (l, a))
  deriving (Eq, Ord, Show, Functor, Foldable, Generic1)
  deriving (Show1) via FunctorClassesDefault (Expr l)

free :: forall ann l. (Ord l) => CF.Cofree (Expr l) ann -> S.Set l
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
    goBranch :: [l] -> S.Set l -> S.Set l
    goBranch xs ys = S.difference ys (S.fromList xs)
