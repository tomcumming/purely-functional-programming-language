module PFL.Compile.Qualify (qualify) where

import Control.Comonad (extract)
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CFT
import Control.Monad.RWS (MonadRWS, evalRWS)
import Control.Monad.RWS.Class (asks, local, state)
import Data.Functor ((<&>))
import Data.Functor.Foldable (cataA)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import PFL.Expr.In qualified as In
import PFL.Expr.Qualified qualified as Q

type InExpr ann = CF.Cofree In.Expr ann

type QExpr ann = CF.Cofree (Q.Expr T.Text (Q.Local T.Text)) ann

type QVal ann = Q.Val T.Text (Q.Local T.Text) (QExpr ann)

qualify :: forall ann. InExpr ann -> QExpr ann
qualify = \inExpr ->
  fst $
    evalRWS (cataA alg inExpr) (mempty :: S.Set T.Text) 0
  where
    alg ::
      (MonadRWS (S.Set T.Text) () Int m) =>
      CFT.CofreeF In.Expr ann (m (QExpr ann)) ->
      m (QExpr ann)
    alg = \case
      ann CFT.:< In.EVar x ->
        asks (S.member x) <&> \case
          True -> ann CF.:< Q.Val (Q.Local (Q.Named x))
          False -> ann CF.:< Q.Val (Q.Global x)
      ann CFT.:< In.Abs x me ->
        (ann CF.:<) . Q.Val . Q.Abs (Q.Named x)
          <$> local (S.insert x) me
      ann CFT.:< In.Ap me1 me2 -> do
        (ef1, v1) <- flatten =<< me1
        -- ef2 may clober some names in ef1 so we write into a fresh var
        l1 <- Q.Anon <$> freshLocal
        (ef2, v2) <- flatten =<< me2
        l <- Q.Anon <$> freshLocal
        let el = ann CF.:< Q.Val (Q.Local l)
        pure $
          ef1 $
            assignValue l1 v1 ann $
              ef2 $
                ann CF.:< Q.Ap (Q.Local l1) v2 l el
      ann CFT.:< In.Match me mbs -> do
        (ef, v) <- flatten =<< me
        bs <- traverse (uncurry goBranch) mbs
        pure $ ef $ ann CF.:< Q.Match v bs

    -- TODO handle duplicate names in pattern
    goBranch xs me = (Q.Named <$> xs,) <$> local (<> S.fromList xs) me

assignValue :: Q.Local T.Text -> QVal ann -> ann -> QExpr ann -> QExpr ann
assignValue l v ann e = ann CF.:< Q.Match v (M.singleton Nothing ([l], e))

freshLocal :: (MonadRWS (S.Set T.Text) () Int m) => m Int
freshLocal = state $ \n -> (n, succ n)

flatten ::
  forall ann m.
  (MonadRWS (S.Set T.Text) () Int m) =>
  QExpr ann ->
  m (QExpr ann -> QExpr ann, QVal ann)
flatten = \case
  _ann CF.:< Q.Val v -> pure (id, v)
  ann CF.:< Q.Ap v1 v2 x e -> do
    (ef, v) <- flatten e
    pure ((ann CF.:<) . Q.Ap v1 v2 x . ef, v)
  ann CF.:< Q.Match v bs -> do
    l <- Q.Anon <$> freshLocal
    bfs <- traverse (uncurry $ goBranch l) bs
    let ef e = ann CF.:< Q.Match v (($ e) <$> bfs)
    pure (ef, Q.Local l)
  where
    goBranch ::
      Q.Local T.Text ->
      [Q.Local T.Text] ->
      QExpr ann ->
      m (QExpr ann -> ([Q.Local T.Text], QExpr ann))
    goBranch l xs e = do
      (ef, v) <- flatten e
      pure $ \e' ->
        ( xs,
          ef $ assignValue l v (extract e) e'
        )
