module PF.Check.Kind (KindF (..)) where

import Data.Functor.Classes (Eq1, Show1)
import Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import GHC.Generics (Generic1)

data KindF a = Ty | Arr a a
  deriving (Generic1, Functor, Foldable, Traversable, Eq, Show)
  deriving (Eq1, Show1) via FunctorClassesDefault KindF
