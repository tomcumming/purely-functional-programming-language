module PF.Compile.LambdaLift
  ( lambdaLift,
    QExpr,
    Ann,
    annotateFree,
    lookupVar,
    growSubst,
    shrinkFrees,
  )
where

import Control.Category ((>>>))
import Control.Comonad (extract)
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CFF
import Control.Monad (forM, (>=>))
import Control.Monad.Reader (MonadReader, asks, local, runReader)
import Data.Function ((&))
import Data.Functor.Foldable (cata)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Sq
import Data.Set qualified as S
import PF.Expr.LambdaLifted qualified as L
import PF.Expr.Qualified qualified as Q

type QExpr g ann = CF.Cofree (Q.Expr g) ann

type LExpr g ann = CF.Cofree (L.Expr g) ann

type Ann ann = (S.Set Q.Idx, ann)

lambdaLift :: QExpr g ann -> LExpr g ann
lambdaLift = annotateFree >>> lambdaLift' >>> flip runReader mempty

annotateFree :: QExpr g ann -> QExpr g (Ann ann)
annotateFree = cata $ \(ann CFF.:< inExpr) -> case inExpr of
  Q.Local l -> (S.singleton l, ann) CF.:< Q.Local l
  Q.Global g -> (mempty, ann) CF.:< Q.Global g
  Q.Abs e -> (shrinkFrees 1 (extractFree e), ann) CF.:< Q.Abs e
  Q.Ap e1 e2 -> (extractFree e1 <> extractFree e2, ann) CF.:< Q.Ap e1 e2
  Q.Match e1 bs ->
    let fbs = flip foldMap bs $ \(n, e) -> shrinkFrees n (extractFree e)
     in (extractFree e1 <> fbs, ann) CF.:< Q.Match e1 bs
  where
    extractFree :: QExpr g (Ann ann) -> S.Set Q.Idx
    extractFree = extract >>> fst

lambdaLift' ::
  forall g ann m.
  (MonadReader (Sq.Seq Q.Idx) m) =>
  QExpr g (Ann ann) ->
  m (LExpr g ann)
lambdaLift' = cata $ \((us, ann) CFF.:< inExpr) -> case inExpr of
  Q.Local l -> lookupVar' ann l
  Q.Global g -> pure $ ann CF.:< L.Global g
  Q.Abs me -> do
    e' <- local (substCls us >>> growSubst (S.size us + 1)) me
    cls <- S.toList us & traverse (lookupVar' ann)
    pure $ ann CF.:< L.Closure cls e'
  Q.Ap me1 me2 -> (ann CF.:<) <$> (L.Ap <$> me1 <*> me2)
  Q.Match me1 mbs -> do
    e1 <- me1
    bs <- forM mbs $ \(n, me) -> (n,) <$> local (growSubst (fromIntegral n)) me
    pure $ ann CF.:< L.Match e1 bs
  where
    substCls :: S.Set Q.Idx -> Sq.Seq Q.Idx -> Sq.Seq Q.Idx
    substCls us = fmap (\i -> s M.!? i & fromMaybe i)
      where
        s = zip (S.toList us) (toEnum @Q.Idx <$> [1 ..]) & M.fromList

lookupVar :: (MonadReader (Sq.Seq Q.Idx) m) => Q.Idx -> m Q.Idx
lookupVar = fromEnum >>> flip (Sq.!?) >>> (asks >=> maybe (error "OOB") pure)

lookupVar' :: (MonadReader (Sq.Seq Q.Idx) m) => ann -> Q.Idx -> m (LExpr g ann)
lookupVar' ann = lookupVar >=> (L.Local >>> (ann CF.:<) >>> pure)

growSubst :: Int -> Sq.Seq Q.Idx -> Sq.Seq Q.Idx
growSubst n =
  fmap (Q.growIdx (fromIntegral n))
    >>> (([0 .. pred n] & fmap toEnum & Sq.fromList) <>)

shrinkFrees :: Word -> S.Set Q.Idx -> S.Set Q.Idx
shrinkFrees n =
  S.toAscList
    >>> filter (fromEnum >>> (>= fromIntegral n))
    >>> fmap (fromEnum >>> subtract (fromIntegral n) >>> toEnum)
    >>> S.fromAscList
