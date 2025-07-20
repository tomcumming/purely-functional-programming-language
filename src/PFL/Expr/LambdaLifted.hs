module PFL.Expr.LambdaLifted (Expr (..), VarCount, Subs, free, subs) where

import Control.Category ((>>>))
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree (tailF)
import Control.Comonad.Trans.Cofree qualified as CFT
import Control.Monad.Reader (MonadReader, asks, local, runReader)
import Data.Foldable (fold)
import Data.Functor.Classes (Show1)
import Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import Data.Functor.Foldable (cata)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Generics (Generic1)
import PFL.Expr.Qualified (Di, VarCount, unAbs, unAbsCount)

data Expr a
  = Local Di
  | Global T.Text
  | -- | Binding order looks like: [3, 2, 1] 0
    --   aka push each from ctx to stack then arg
    Closure [a] a
  | Ap a a
  | -- | This also handles a traditional (mono) let
    Match a (M.Map T.Text (VarCount, a)) (Maybe a)
  deriving (Eq, Ord, Show, Functor, Foldable, Generic1)
  deriving (Show1) via FunctorClassesDefault Expr

type Subs ann = M.Map Di (CF.Cofree Expr ann)

free :: CF.Cofree Expr ann -> S.Set Di
free =
  cata $
    tailF >>> \case
      Local x -> S.singleton x
      e@Global {} -> fold e
      Closure yss zs -> fold yss <> unAbsCount (toEnum (length yss + 1)) zs
      e@Ap {} -> fold e
      Match ys bs db ->
        ys
          <> foldMap (uncurry unAbsCount) bs
          <> foldMap unAbs db

subs :: forall ann. Subs ann -> CF.Cofree Expr ann -> CF.Cofree Expr ann
subs s = flip runReader 0 . cata go
  where
    go ::
      forall m.
      (MonadReader Int m) =>
      CFT.CofreeF Expr ann (m (CF.Cofree Expr ann)) -> m (CF.Cofree Expr ann)
    go = \case
      ann CFT.:< Local x -> asks $ \case
        k | Just e <- s M.!? toEnum (fromEnum x - k) -> e
        _ -> ann CF.:< Local x
      ann CFT.:< Global x -> pure $ ann CF.:< Global x
      ann CFT.:< Closure ctx e ->
        (ann CF.:<)
          <$> (Closure <$> sequence ctx <*> local (+ succ (length ctx)) e)
      ann CFT.:< Ap me1 me2 -> (ann CF.:<) <$> (Ap <$> me1 <*> me2)
      ann CFT.:< Match me mbs mdb -> do
        e <- me
        bs <- traverse (uncurry goBranch) mbs
        db <- traverse (local succ) mdb
        pure $ ann CF.:< Match e bs db
        where
          goBranch c mb = (c,) <$> local (+ fromEnum c) mb
