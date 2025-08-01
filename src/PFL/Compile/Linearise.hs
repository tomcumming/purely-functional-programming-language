module PFL.Compile.Linearise (linearise, Names (..)) where

import Control.Comonad (extract)
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CFT
import Control.Monad.State (evalState)
import Control.Monad.State.Class (MonadState, state)
import Data.Bitraversable (secondA)
import Data.Functor.Foldable (cataA)
import Data.Map qualified as M
import Data.Set qualified as S
import PFL.Expr.Qualified qualified as Q

data Names g = Names
  { nmDrop :: g,
    nmCopy :: g,
    nmUnit :: g,
    nmPair :: g
  }

type QExpr g l ann = CF.Cofree (Q.Expr g (Q.Local l)) ann

linearise ::
  forall g l ann.
  (Ord l) =>
  Names g ->
  QExpr g l ann ->
  QExpr g l ann
linearise nms = \inExpr ->
  let xf = maybe 0 succ $ Q.maxAnon inExpr
   in snd $ evalState (cataA alg inExpr) xf
  where
    alg ::
      (MonadState Word m, ret ~ (M.Map (Q.Local l) ann, QExpr g l ann)) =>
      CFT.CofreeF (Q.Expr g (Q.Local l)) ann (m ret) ->
      m ret
    alg = \case
      ann CFT.:< Q.Local x -> pure (M.singleton x ann, ann CF.:< Q.Local x)
      ann CFT.:< Q.Global x -> pure (mempty, ann CF.:< Q.Global x)
      ann CFT.:< Q.Abs x me -> do
        (fs, e) <- me
        let drops = doDrops nms (S.difference (S.singleton x) (M.keysSet fs))
        pure (M.delete x fs, ann CF.:< Q.Abs x (drops e))
      ann CFT.:< Q.Ap me1 me2 -> do
        (fs1, e1) <- me1
        (fs2, e2) <- me2
        (ef, e1') <- copyInFirstExpr fs1 fs2 e1
        pure (fs2 <> fs1, ef $ ann CF.:< Q.Ap e1' e2)
      ann CFT.:< Q.Match me1 mbs -> do
        (fs1, e1) <- me1
        bs <- traverse (secondA id) mbs
        let fs2 = foldMap (fst . snd) bs
        (ef, e1') <- copyInFirstExpr fs1 fs2 e1
        let bs' = uncurry (branchDrops fs2) <$> bs
        pure (fs2 <> fs1, ef $ ann CF.:< Q.Match e1' bs')

    copyInFirstExpr ::
      (MonadState Word m) =>
      M.Map (Q.Local l) ann ->
      M.Map (Q.Local l) ann ->
      QExpr g l ann ->
      m (QExpr g l ann -> QExpr g l ann, QExpr g l ann)
    copyInFirstExpr fs1 fs2 e1 = do
      let dupes = M.intersectionWith const fs1 fs2
      s <-
        traverse
          (\ann' -> (ann',) . Q.Anon <$> fresh)
          dupes
      let e1' = Q.subst ((\(ann', x) -> ann' CF.:< Q.Local x) <$> s) e1
      let ef = bindSubst nms (snd <$> s)
      pure (ef, e1')

    branchDrops ::
      M.Map (Q.Local l) ann ->
      [Q.Local l] ->
      (M.Map (Q.Local l) ann, QExpr g l ann) ->
      ([Q.Local l], CF.Cofree (Q.Expr g (Q.Local l)) ann)
    branchDrops afs xs (fs, e) =
      ( xs,
        doDrops nms (M.keysSet (M.difference afs fs)) e
      )

bindSubst ::
  forall g l ann.
  Names g ->
  M.Map (Q.Local l) (Q.Local l) ->
  QExpr g l ann ->
  QExpr g l ann
bindSubst nms@Names {nmPair} = flip M.foldrWithKey id $
  \x x' prev -> go x x' . prev
  where
    go :: Q.Local l -> Q.Local l -> QExpr g l ann -> QExpr g l ann
    go x x' e =
      ann
        CF.:< Q.Match
          (callCopy nms ann x)
          (M.singleton (Just nmPair) ([x, x'], e))
      where
        ann = extract e

fresh :: (MonadState Word m) => m Word
fresh = state $ \x -> (x, succ x)

doDrops :: Names g -> S.Set (Q.Local l) -> QExpr g l ann -> QExpr g l ann
doDrops nms@Names {nmUnit} xs e = foldr go e xs
  where
    ann = extract e

    go x e' =
      ann
        CF.:< Q.Match
          (callDrop nms ann x)
          (M.singleton (Just nmUnit) ([], e'))

callDrop :: Names g -> ann -> Q.Local l -> QExpr g l ann
callDrop Names {nmDrop} = callFn nmDrop

callCopy :: Names g -> ann -> Q.Local l -> QExpr g l ann
callCopy Names {nmCopy} = callFn nmCopy

callFn :: g -> ann -> Q.Local l -> QExpr g l ann
callFn f ann x =
  ann
    CF.:< Q.Ap
      (ann CF.:< Q.Global f)
      (ann CF.:< Q.Local x)
