module PFL.Typing.Type (Ty (..)) where

import Data.Functor.Classes (Eq1, Ord1, Show1)
import Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import GHC.Generics (Generic1)

data Ty k c a
  = Var c
  | Ap a a
  | Forall c k a
  deriving (Eq, Ord, Show, Functor, Foldable, Generic1)
  deriving (Eq1, Ord1, Show1) via FunctorClassesDefault (Ty k c)
