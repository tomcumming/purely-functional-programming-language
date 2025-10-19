module PF.Check.Type (Typ (..)) where

import Data.Functor.Classes (Eq1, Ord1, Show1)
import Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import GHC.Generics (Generic1)

data Typ c a
  = Con c
  | Forall a a
  | Ap a a
  deriving (Generic1, Functor, Foldable, Eq, Ord, Show)
  deriving (Eq1, Ord1, Show1) via (FunctorClassesDefault (Typ c))
