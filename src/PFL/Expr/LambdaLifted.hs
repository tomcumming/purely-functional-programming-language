module PFL.Expr.LambdaLifted (Expr (..)) where

import Data.Functor.Classes (Show1)
import Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import Data.Map qualified as M
import GHC.Generics (Generic1)

data Expr g l a
  = Local l
  | Global g
  | Closure a l a
  | Ap a a
  | Match a (M.Map (Maybe g) ([l], a))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic1)
  deriving (Show1) via FunctorClassesDefault (Expr g l)
