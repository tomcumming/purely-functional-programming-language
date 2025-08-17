module PFL.Compile.Thread (thread, Names (..)) where

import Control.Comonad (extract)
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CFT
import Control.Monad ((>=>))
import Control.Monad.State (evalState)
import Control.Monad.State.Class (MonadState, state)
import Data.Bifunctor (second)
import Data.Functor.Foldable (cata, cataA)
import Data.Map qualified as M
import PFL.Expr.Qualified qualified as Q

data Names g = Names
  { nmThreaded :: g,
    nmThread :: g,
    nmPair :: g
  }

type QExpr g l ann = CF.Cofree (Q.Expr g (Q.Local l)) ann

thread :: forall g l ann. (Ord g) => Names g -> QExpr g l ann -> QExpr g l ann
thread nms@Names {nmThreaded} = cata alg
  where
    alg = \case
      ann CFT.:< Q.Local x -> ann CF.:< Q.Local x
      ann CFT.:< Q.Global x -> ann CF.:< Q.Global x
      ann CFT.:< Q.Abs x e -> ann CF.:< Q.Abs x e
      ann CFT.:< Q.Ap e1 e2 -> case e1 of
        _ann CF.:< Q.Global x
          | x == nmThreaded ->
              let w = maybe 0 succ $ Q.maxAnon e2
                  e2' = evalState (thread' nms (Q.Anon w) e2) (succ w)
               in ann CF.:< Q.Abs (Q.Anon w) e2'
        _ -> ann CF.:< Q.Ap e1 e2
      ann CFT.:< Q.Match e bs -> ann CF.:< Q.Match e bs

data Threading = Simple | Threaded deriving (Eq)

thread' ::
  forall g l ann m.
  (Ord g, MonadState Word m) =>
  Names g ->
  Q.Local l ->
  QExpr g l ann ->
  m (QExpr g l ann)
thread' Names {nmPair, nmThread} w =
  cataA alg >=> \case
    (Simple, e) -> pure $ pairWithCtx e
    (Threaded, e) -> pure e
  where
    alg ::
      CFT.CofreeF
        (Q.Expr g (Q.Local l))
        ann
        (m (Threading, QExpr g l ann)) ->
      m (Threading, QExpr g l ann)
    alg = \case
      ann CFT.:< Q.Local x -> pure (Simple, ann CF.:< Q.Local x)
      ann CFT.:< Q.Global x -> pure (Simple, ann CF.:< Q.Global x)
      ann CFT.:< Q.Abs x me -> do
        e <- me
        pure $ (ann CF.:<) . Q.Abs x <$> e
      ann CFT.:< Q.Ap me1 me2 -> do
        te1 <- me1
        te2 <- me2
        case (te1, te2) of
          ((Simple, _ CF.:< Q.Global x), (_, e2))
            | x == nmThread ->
                pure
                  ( Threaded,
                    ann CF.:< Q.Ap e2 (ann CF.:< Q.Local w)
                  )
          ((Simple, e1), (Simple, e2)) -> pure (Simple, ann CF.:< Q.Ap e1 e2)
          _ -> do
            (ef1, e1') <- threadSub te1
            (ef2, e2') <- threadSub te2
            pure
              ( Threaded,
                ef1 $ ef2 $ pairWithCtx $ ann CF.:< Q.Ap e1' e2'
              )
      ann CFT.:< Q.Match me mbs -> do
        te1 <- me
        bs <- traverse (uncurry goBranch) mbs
        let anyThreaded =
              fst te1 == Threaded
                || any ((== Threaded) . fst . snd) bs
        if not anyThreaded
          then
            let bs' = second snd <$> bs
             in pure (Simple, ann CF.:< Q.Match (snd te1) bs')
          else do
            (fe1, e1) <- threadSub te1
            let bs' = uncurry liftBranch <$> bs
            pure
              ( Threaded,
                fe1 $ ann CF.:< Q.Match e1 bs'
              )
      where
        goBranch xs me = (xs,) <$> me

        liftBranch xs =
          (xs,) . \case
            (Threaded, e) -> e
            (Simple, e) -> pairWithCtx e

    pairWithCtx :: QExpr g l ann -> QExpr g l ann
    pairWithCtx e =
      let ann = extract e
       in ann
            CF.:< Q.Ap
              ( ann
                  CF.:< Q.Ap (ann CF.:< Q.Global nmPair) (ann CF.:< Q.Local w)
              )
              e

    fresh :: m Word
    fresh = state $ \x -> (x, succ x)

    threadSub ::
      (Threading, QExpr g l ann) ->
      m (QExpr g l ann -> QExpr g l ann, QExpr g l ann)
    threadSub = \case
      (Simple, e) -> pure (id, e)
      (Threaded, e) -> do
        let ann = extract e
        x <- Q.Anon <$> fresh
        let ef e' =
              ann
                CF.:< Q.Match
                  (ann CF.:< Q.Ap e (ann CF.:< Q.Local w))
                  (M.singleton (Just nmPair) ([w, x], e'))
        pure (ef, ann CF.:< Q.Local x)
