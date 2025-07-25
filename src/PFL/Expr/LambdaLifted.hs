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

data Expr g l a
  = Local l
  | Global g
  | Closure a l a
  | Ap a a
  | Match a (M.Map (Maybe T.Text) ([l], a))
  deriving (Eq, Ord, Show, Functor, Foldable, Generic1)
  deriving (Show1) via FunctorClassesDefault (Expr g l)

free :: forall ann g l. (Ord l) => CF.Cofree (Expr g l) ann -> S.Set l
free =
  cata $
    tailF >>> \case
      Local x -> S.singleton x
      e@Global {} -> fold e
      Closure ys x zs -> ys <> S.delete x zs
      e@Ap {} -> fold e
      Match ys bs -> ys <> foldMap (uncurry goBranch) bs
  where
    goBranch :: [l] -> S.Set l -> S.Set l
    goBranch xs ys = S.difference ys (S.fromList xs)
