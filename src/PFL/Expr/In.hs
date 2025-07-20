module PFL.Expr.In (Expr (..), free) where

import Control.Category ((>>>))
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree (tailF)
import Data.Foldable (fold)
import Data.Functor.Classes (Show1)
import Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import Data.Functor.Foldable (cata)
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Generics (Generic1)

data Expr a = EVar T.Text | Abs T.Text a | Ap a a
  deriving (Eq, Ord, Show, Functor, Foldable, Generic1)
  deriving (Show1) via FunctorClassesDefault Expr

free :: CF.Cofree Expr ann -> S.Set T.Text
free =
  cata $
    tailF >>> \case
      EVar x -> S.singleton x
      Abs x xs -> S.delete x xs
      e@Ap {} -> fold e
