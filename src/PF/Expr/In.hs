module PF.Expr.In
  ( PatF (..),
    Pat,
    ExprF (..),
  )
where

import Data.Fix (Fix (..))
import Data.Functor.Classes (Show1)
import Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import GHC.Generics (Generic1)

data PatF n a
  = Blank
  | Named n
  | Cons n [a]
  deriving (Generic1, Functor, Show)
  deriving (Show1) via FunctorClassesDefault (PatF n)

type Pat n = Fix (PatF n)

data ExprF n a
  = Var n
  | Abs (Pat n) a
  | Ap a a
  | Match a [(Pat n, a)]
  deriving (Generic1, Functor, Show)
  deriving (Show1) via FunctorClassesDefault (ExprF n)
