module PF.Expr.In
  ( ExprF (..),
  )
where

import Data.Functor.Classes (Show1)
import Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import GHC.Generics (Generic1)

-- TODO case/match expressions

data ExprF x a
  = Var x
  | Comp a a
  | Lambda a
  deriving (Generic1, Functor, Show)
  deriving (Show1) via FunctorClassesDefault (ExprF x)
