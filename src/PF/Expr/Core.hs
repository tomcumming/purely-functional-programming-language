module PF.Expr.Core
  ( Idx,
    PatF (..),
    Pat,
    ExprF (..),
  )
where

import Data.Fix (Fix)
import Data.Functor.Classes (Show1)
import Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import GHC.Generics (Generic1)

newtype Idx = Idx Int deriving newtype (Eq, Ord, Enum, Show)

data PatF c a
  = Var
  | Con c
  | PAp a a
  deriving (Generic1, Functor, Show)
  deriving (Show1) via FunctorClassesDefault (PatF c)

type Pat c = Fix (PatF c)

data ExprF c a
  = Local Idx
  | Global c
  | Lambda (Pat c) a
  | Ap a a
  deriving (Generic1, Functor, Show)
  deriving (Show1) via FunctorClassesDefault (ExprF c)
