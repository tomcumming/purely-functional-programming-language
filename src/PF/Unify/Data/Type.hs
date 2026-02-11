module PF.Unify.Data.Type
  ( Idx,
    TyF (..),
  )
where

import Data.Functor.Classes (Eq1, Ord1, Show1)
import Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import GHC.Generics (Generic1)

newtype Idx = Idx Int deriving newtype (Eq, Ord, Enum, Show)

data TyF k c a
  = Var Idx
  | Con c
  | For k a
  | Ap a a
  deriving (Generic1, Functor, Foldable, Traversable, Eq, Ord, Show)
  deriving (Eq1, Ord1, Show1) via FunctorClassesDefault (TyF k c)
