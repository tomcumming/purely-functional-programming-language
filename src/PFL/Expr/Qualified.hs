module PFL.Expr.Qualified (Expr (..), free) where

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
  | Abs l a
  | Ap a a
  | Match a (M.Map T.Text ([l], a)) (Maybe (l, a))
  deriving (Eq, Ord, Show, Functor, Foldable, Generic1)
  deriving (Show1) via FunctorClassesDefault (Expr l)

free :: forall ann l. (Ord l) => CF.Cofree (Expr l) ann -> S.Set l
free =
  cata $
    tailF >>> \case
      Local x -> S.singleton x
      Abs x xs -> S.delete x xs
      Match e bs db ->
        e
          <> foldMap (uncurry goBranch) bs
          <> foldMap (uncurry S.delete) db
      e -> fold e
  where
    goBranch :: [l] -> S.Set l -> S.Set l
    goBranch xs ys = S.difference ys (S.fromList xs)
