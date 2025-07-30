module PFL.Expr.Qualified (Local (..), Expr (..), Val (..)) where

import Data.Functor.Classes (Show1)
import Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import Data.Map qualified as M
import GHC.Generics (Generic1)

data Local l
  = Named l
  | Anon Int
  deriving (Eq, Ord, Show)

data Expr g l a
  = Val (Val g l a)
  | Ap (Val g l a) (Val g l a) l a
  | Match (Val g l a) (M.Map (Maybe g) ([l], a))
  deriving (Eq, Ord, Show, Functor, Foldable, Generic1)
  deriving (Show1) via FunctorClassesDefault (Expr g l)

data Val g l a
  = Local l
  | Global g
  | Abs l a
  deriving (Eq, Ord, Show, Functor, Foldable, Generic1)
  deriving (Show1) via FunctorClassesDefault (Val g l)
