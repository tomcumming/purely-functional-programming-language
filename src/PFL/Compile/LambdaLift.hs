module PFL.Compile.LambdaLift
  ( lambdaLift,
  )
where

import Control.Category ((>>>))
import Control.Comonad (extract)
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree (tailF)
import Control.Comonad.Trans.Cofree qualified as CFT
import Control.Monad.State (MonadState, evalState, state)
import Data.Bitraversable (secondA)
import Data.Foldable (fold)
import Data.Functor.Foldable (cata, cataA)
import Data.List (unsnoc)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import PFL.Compile.Linearise (Linearised, Uid, Unique (..), unLinearised)
import PFL.Expr.LambdaLifted qualified as L
import PFL.Expr.Qualified qualified as Q

type LExpr ann = CF.Cofree (L.Expr T.Text Unique) ann

lambdaLift :: forall ann. Linearised ann -> LExpr ann
lambdaLift = foo . Q.annotateFree . unLinearised
  where
    foo e = flip evalState (firstFresh e) $ cataA go e

    go ::
      (MonadState Uid m) =>
      CFT.CofreeF (Q.Expr T.Text Unique) (S.Set Unique, ann) (m (LExpr ann)) ->
      m (LExpr ann)
    go ((fs, ann) CFT.:< e) = case e of
      Q.Local x -> pure $ ann CF.:< L.Local x
      Q.Global x -> pure $ ann CF.:< L.Global x
      Q.Abs x me' -> do
        e' <- bindClosure fs x =<< me'
        pure $ ann CF.:< L.Closure (makeClosure ann fs) x e'
      Q.Ap me1 me2 -> do
        e1 <- me1
        e2 <- me2
        pure $ ann CF.:< L.Ap e1 e2
      Q.Match me' mbs -> do
        e' <- me'
        bs <- traverse (secondA id) mbs
        pure $ ann CF.:< L.Match e' bs

firstFresh :: CF.Cofree (Q.Expr T.Text Unique) ann -> Uid
firstFresh = maybe (toEnum 0) succ . S.lookupMax . cata go
  where
    uid (Unique _ u) = u

    go =
      tailF >>> \case
        Q.Local u -> S.singleton $ uid u
        Q.Global {} -> mempty
        Q.Abs u us -> S.insert (uid u) us
        e@Q.Ap {} -> fold e
        Q.Match us bs ->
          us
            <> foldMap
              (\(xs, us') -> foldMap (S.singleton . uid) xs <> us')
              bs

makeClosure :: forall ann. ann -> S.Set Unique -> LExpr ann
makeClosure ann =
  S.toList >>> unsnoc >>> \case
    Nothing -> ann CF.:< L.Global "Unit"
    Just (xs, x) -> foldr pairUp (ann CF.:< L.Local x) xs
  where
    pairUp :: Unique -> LExpr ann -> LExpr ann
    pairUp x e =
      ann
        CF.:< L.Ap
          (ann CF.:< L.Ap (ann CF.:< L.Global "Pair") (ann CF.:< L.Local x))
          e

fresh :: (MonadState Uid m) => m Uid
fresh = state $ \u -> (u, succ u)

bindClosure ::
  (MonadState Uid m) =>
  S.Set Unique ->
  Unique ->
  LExpr ann ->
  m (LExpr ann)
bindClosure fs x originalBody = do
  ctx <- Unique "ctx" <$> fresh
  eBody <- bindClosureContext ann ctx originalBody (S.toList fs)
  pure $
    ann
      CF.:< L.Match
        (ann CF.:< L.Local x)
        (M.singleton (Just "Pair") ([x, ctx], eBody))
  where
    ann = extract originalBody

bindClosureContext ::
  (MonadState Uid m) =>
  ann ->
  Unique ->
  LExpr ann ->
  [Unique] ->
  m (LExpr ann)
bindClosureContext ann u eBody fs = do
  bs <- case fs of
    [] -> pure $ M.singleton (Just "Unit") ([], eBody)
    [x] -> pure $ M.singleton Nothing ([x], eBody)
    x : xs -> do
      ctx <- Unique "ctx" <$> fresh
      eBody' <- bindClosureContext ann ctx eBody xs
      pure $ M.singleton (Just "Pair") ([x, ctx], eBody')
  pure $ ann CF.:< L.Match (ann CF.:< L.Local u) bs
