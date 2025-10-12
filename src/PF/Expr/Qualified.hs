module PF.Expr.Qualified (Idx, Expr (..)) where

import Control.Category ((>>>))
import Data.Functor.Classes (Show1)
import Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import Data.Map qualified as M
import GHC.Generics (Generic1)

-- | Debruijn index
newtype Idx = Idx {unIdx :: Int}
  deriving stock (Eq, Ord)
  deriving newtype (Enum)

instance Show Idx where show = unIdx >>> show >>> ("i" <>)

data Expr g a
  = Local Idx
  | Global g
  | Abs a -- introduces one value
  | Ap a a
  | Match a (M.Map (Maybe g) (Word, a))
  deriving stock (Show, Functor, Generic1)
  deriving (Show1) via FunctorClassesDefault (Expr g)
