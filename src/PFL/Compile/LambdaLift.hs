module PFL.Compile.LambdaLift (lambdaLift, lambdaLift', Names (..)) where

import Control.Category ((>>>))
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CFT
import Control.Monad.Reader (runReader)
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Data.Bifunctor (first)
import Data.Containers.ListUtils (nubOrd)
import Data.Functor.Foldable (cataA)
import Data.Map qualified as M
import Data.Set qualified as S
import PFL.Expr.LambdaLifted qualified as L
import PFL.Expr.Qualified qualified as Q

data Names g = Names
  { nmUnit :: g,
    nmPair :: g
  }

type QExpr g l ann = CF.Cofree (Q.Expr g (Q.Local l)) ann

type LExpr g l ann = CF.Cofree (L.Expr g (Q.Local l)) ann

lambdaLift ::
  forall g l ann.
  (Ord l) =>
  Names g ->
  QExpr g l ann ->
  LExpr g l ann
lambdaLift nms = flip runReader [] . lambdaLift' nms

-- | Input expression must be linear
lambdaLift' ::
  forall g l ann m.
  (Ord l, MonadReader [Q.Local l] m) =>
  Names g ->
  QExpr g l ann ->
  m (LExpr g l ann)
lambdaLift' Names {nmPair, nmUnit} inExpr = snd <$> cataA algMain inExpr
  where
    algMain ::
      CFT.CofreeF
        (Q.Expr g (Q.Local l))
        ann
        (m (S.Set (Q.Local l), LExpr g l ann)) ->
      m (S.Set (Q.Local l), LExpr g l ann)
    algMain = \case
      ann CFT.:< Q.Local l -> pure (S.singleton l, ann CF.:< L.Local l)
      ann CFT.:< Q.Global g -> pure (mempty, ann CF.:< L.Global g)
      ann CFT.:< Q.Abs x me -> do
        (ls, e) <- first (S.delete x) <$> local (x :) me
        ctx <- asks $ makeContext ls
        pure
          ( ls,
            ann CF.:< L.Closure (pairUp ann ctx) x (unPair ann x e ctx)
          )
      ann CFT.:< Q.Ap me1 me2 -> do
        (ls1, e1) <- me1
        (ls2, e2) <- me2
        pure (ls1 <> ls2, ann CF.:< L.Ap e1 e2)
      ann CFT.:< Q.Match me1 mbs -> do
        (ls1, e1) <- me1
        bs <-
          traverse
            ( \(xs, me) -> do
                (ls, e) <- first (`S.difference` S.fromList xs) <$> local (xs <>) me
                pure (ls, (xs, e))
            )
            mbs
        let ls2 = foldMap fst bs
        let bs' = snd <$> bs
        pure (ls1 <> ls2, ann CF.:< L.Match e1 bs')

    makeContext :: S.Set (Q.Local l) -> [Q.Local l] -> [Q.Local l]
    makeContext ls = filter (`S.member` ls) >>> nubOrd

    fresh :: Q.Local l
    fresh = Q.Anon $ maybe 0 succ $ Q.maxAnon inExpr

    pairUp :: ann -> [Q.Local l] -> LExpr g l ann
    pairUp ann = \case
      [] -> ann CF.:< L.Global nmUnit
      lMax : ls ->
        foldl'
          (\e l -> callPair ann (ann CF.:< L.Local l) e)
          (ann CF.:< L.Local lMax)
          ls

    unPair ::
      ann ->
      Q.Local l ->
      LExpr g l ann ->
      [Q.Local l] ->
      LExpr g l ann
    unPair ann lArg eBody = \case
      [] ->
        unPairArg lArg fresh lArg $
          ann
            CF.:< L.Match
              (ann CF.:< L.Local fresh)
              (M.singleton (Just nmUnit) ([], eBody))
      [lMax] -> unPairArg lArg lMax lArg eBody
      lMax : lMax2 : ls ->
        unPairArg lArg fresh lArg $
          foldl
            (\e l -> unPairArg fresh l fresh e)
            (unPairArg fresh lMax2 lMax eBody)
            ls
      where
        unPairArg l1 l2 l3 e =
          ann
            CF.:< L.Match
              (ann CF.:< L.Local l1)
              (M.singleton (Just nmPair) ([l2, l3], e))

    callPair ann e1 e2 = ann CF.:< L.Ap (callFn ann nmPair e1) e2
    callFn ann f e = ann CF.:< L.Ap (ann CF.:< L.Global f) e
