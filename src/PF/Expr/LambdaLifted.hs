module PF.Expr.LambdaLifted (Expr (..)) where

import Data.Functor.Classes (Show1)
import Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import Data.Map qualified as M
import GHC.Generics (Generic1)
import PF.Expr.Qualified (Idx)

data Expr g a
  = Local Idx
  | Global g
  | Closure [a] a
  | Ap a a
  | Match a (M.Map (Maybe g) (Word, a))
  deriving stock (Show, Functor, Generic1)
  deriving (Show1) via FunctorClassesDefault (Expr g)
