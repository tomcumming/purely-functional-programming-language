module PFL.Expr.Qualified
  ( Expr (..),
    Di,
    VarCount,
    unAbs,
    unAbsCount,
    free,
  )
where

import Control.Category ((>>>))
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree (tailF)
import Data.Foldable (fold)
import Data.Functor.Classes (Show1)
import Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import Data.Functor.Foldable (cata)
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Generics (Generic1)

-- | Debruijn index
newtype Di = Di Int
  deriving newtype (Eq, Ord, Show, Enum)

newtype VarCount = VarCount Int
  deriving newtype (Eq, Ord, Show, Enum)

unAbs :: S.Set Di -> S.Set Di
unAbs = S.filter (>= toEnum 0) . S.mapMonotonic pred

unAbsCount :: VarCount -> S.Set Di -> S.Set Di
unAbsCount c xs
  | c <= toEnum 0 = xs
  | otherwise = unAbsCount (pred c) (unAbs xs)

data Expr a
  = Local Di
  | Global T.Text
  | Abs a
  | Ap a a
  deriving (Eq, Ord, Show, Functor, Foldable, Generic1)
  deriving (Show1) via FunctorClassesDefault Expr

free :: CF.Cofree Expr ann -> S.Set Di
free =
  cata $
    tailF >>> \case
      Local x -> S.singleton x
      Abs xs -> S.filter (>= toEnum 0) $ S.map pred xs
      e -> fold e
