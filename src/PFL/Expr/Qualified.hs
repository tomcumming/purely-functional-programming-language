module PFL.Expr.Qualified (Expr (..), annotateFree) where

import Control.Comonad (extract)
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CFT
import Data.Functor.Classes (Show1)
import Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import Data.Functor.Foldable (cata)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Generics (Generic1)

data Expr g l a
  = Local l
  | Global g
  | Abs l a
  | Ap a a
  | Match a (M.Map (Maybe T.Text) ([l], a))
  deriving (Eq, Ord, Show, Functor, Foldable, Generic1)
  deriving (Show1) via FunctorClassesDefault (Expr g l)

annotateFree ::
  forall g l ann.
  (Ord l) =>
  CF.Cofree (Expr g l) ann ->
  CF.Cofree (Expr g l) (S.Set l, ann)
annotateFree = cata $ \(ann CFT.:< e) -> case e of
  Local x -> (S.singleton x, ann) CF.:< Local x
  Global x -> (mempty, ann) CF.:< Global x
  Abs x e' ->
    let f = S.delete x $ fst $ extract e'
     in (f, ann) CF.:< Abs x e'
  Ap e1 e2 ->
    let f = fst (extract e1) <> fst (extract e2)
     in (f, ann) CF.:< Ap e1 e2
  Match e' bs ->
    let f = fst (extract e') <> foldMap (uncurry goBranch) bs
     in (f, ann) CF.:< Match e' bs
  where
    goBranch :: [l] -> CF.Cofree (Expr g l) (S.Set l, ann) -> S.Set l
    goBranch xs ((fs, _) CF.:< _) = S.difference fs (S.fromList xs)
