module PF.Expr.Linearised
  ( PatF (..),
    Pat,
    ExprF (..),
  )
where

import Data.Fix (Fix)
import Data.Functor.Classes (Show1)
import Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import GHC.Generics (Generic1)

data PatF l g a
  = Var l
  | Cons g [a]
  deriving (Generic1, Functor, Show)
  deriving (Show1) via FunctorClassesDefault (PatF l g)

type Pat l g = Fix (PatF l g)

data ExprF l g a
  = Local l
  | Global g
  | Abs (Pat l g) a
  | Ap a a
  | Match a [(Pat l g, a)]
  deriving (Generic1, Functor, Show)
  deriving (Show1) via FunctorClassesDefault (ExprF l g)
