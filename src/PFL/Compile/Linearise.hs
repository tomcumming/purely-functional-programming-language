module PFL.Compile.Linearise
  ( linearise,
    Linearised,
    Names (..),
    unLinearised,
  )
where

import Control.Comonad (extract)
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CFT
import Control.Monad.RWS (evalRWS)
import Control.Monad.RWS.Class (MonadRWS, MonadWriter, censor, gets, listen, modify, tell)
import Control.Monad.State (MonadState)
import Data.Functor.Foldable (cataA)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import PFL.Expr.Qualified qualified as Q

newtype Linearised g ann = Linearised
  {unLinearised :: CF.Cofree (Q.Expr g T.Text) ann}

data Names g = Names
  { nmUnit :: !g,
    nmPair :: !g,
    nmCopy :: !g,
    nmDrop :: !g
  }

-- TODO we need to catch and report on patterns with duplicate names:
--   \(a, a) -> ...

newtype NameMap l = NameMap (M.Map l (S.Set l))

instance (Ord l) => Semigroup (NameMap l) where
  NameMap nm1 <> NameMap nm2 = NameMap $ M.unionWith (<>) nm1 nm2

instance (Ord l) => Monoid (NameMap l) where mempty = NameMap mempty

mapName :: l -> l -> NameMap l
mapName x x' = NameMap $ M.singleton x $ S.singleton x'

-- | Add drops and copies so every introduced name in the output expression is
-- used linearly.
linearise ::
  forall g ann.
  Names g ->
  CF.Cofree (Q.Expr g T.Text) ann ->
  Linearised g ann
linearise nms =
  \inExpr -> Linearised $ fst $ evalRWS (cataA alg inExpr) nms mempty
  where
    alg ::
      forall m.
      (MonadRWS (Names g) (NameMap T.Text) (S.Set T.Text) m) =>
      CFT.CofreeF (Q.Expr g T.Text) ann (m (CF.Cofree (Q.Expr g T.Text) ann)) ->
      m (CF.Cofree (Q.Expr g T.Text) ann)
    alg (ann CFT.:< inExpr) = case inExpr of
      Q.Local x -> do
        x' <- freshNameLike x
        tell $ mapName x x'
        pure $ ann CF.:< Q.Local x'
      Q.Global x -> pure $ ann CF.:< Q.Global x
      Q.Abs x me -> do
        (e, NameMap nm) <- consume $ withoutLocals (S.singleton x) me
        let drops =
              M.unionWith (<>) (M.singleton x mempty) $
                M.restrictKeys nm (S.singleton x)
        tell $ NameMap $ M.delete x nm
        pure $ ann CF.:< Q.Abs x (doDrop nms drops e)
      Q.Ap me1 me2 -> do
        ((e1, e2), NameMap nm) <- consume $ (,) <$> me1 <*> me2
        let (copies, nm') = M.partition ((> 1) . S.size) nm
        tell $ NameMap nm'
        tell $ NameMap $ M.mapWithKey (\k _ -> S.singleton k) copies
        pure $ doCopy nms copies $ ann CF.:< Q.Ap e1 e2
      Q.Match me bs -> do
        ((e, bs'), NameMap nm) <- consume $ do
          e <- me
          bs' <- traverse (uncurry goBranch) bs
          pure (e, bs')
        let (copies, nm') = M.partition ((> 1) . S.size) nm
        tell $ NameMap nm'
        tell $ NameMap $ M.mapWithKey (\k _ -> S.singleton k) copies
        pure $ doCopy nms copies $ ann CF.:< Q.Match e bs'
      where
        goBranch ::
          [T.Text] ->
          m (CF.Cofree (Q.Expr g T.Text) ann) ->
          m ([T.Text], CF.Cofree (Q.Expr g T.Text) ann)
        goBranch xs me = do
          let xss = S.fromList xs
          (e, NameMap nm) <- consume $ withoutLocals xss me
          let drops =
                M.unionWith (<>) (M.fromSet (const mempty) xss) $
                  M.restrictKeys nm xss
          tell $ NameMap $ M.withoutKeys nm xss
          pure (xs, doDrop nms drops e)

consume :: (MonadWriter (NameMap l) m) => m a -> m (a, NameMap l)
consume = censor (const mempty) . listen

callFn ::
  ann ->
  g ->
  CF.Cofree (Q.Expr g T.Text) ann ->
  CF.Cofree (Q.Expr g T.Text) ann
callFn ann f e = ann CF.:< Q.Ap (ann CF.:< Q.Global f) e

doDrop ::
  forall g ann.
  Names g ->
  M.Map T.Text (S.Set T.Text) ->
  CF.Cofree (Q.Expr g T.Text) ann ->
  CF.Cofree (Q.Expr g T.Text) ann
doDrop Names {nmDrop, nmUnit} = \nm inExpr -> M.foldrWithKey goName inExpr nm
  where
    goName ::
      T.Text ->
      S.Set T.Text ->
      CF.Cofree (Q.Expr g T.Text) ann ->
      CF.Cofree (Q.Expr g T.Text) ann
    goName x xs e
      | S.size xs > 1 = error "TODO explain internal error"
      | S.size xs == 1 = e
      | otherwise =
          ann
            CF.:< Q.Match
              (callFn ann nmDrop (ann CF.:< Q.Local x))
              (M.singleton (Just nmUnit) ([], e))
      where
        ann = extract e

doCopy ::
  forall g ann.
  Names g ->
  M.Map T.Text (S.Set T.Text) ->
  CF.Cofree (Q.Expr g T.Text) ann ->
  CF.Cofree (Q.Expr g T.Text) ann
doCopy Names {nmCopy, nmPair} = \nm inExpr -> M.foldrWithKey goName inExpr nm
  where
    goName ::
      T.Text ->
      S.Set T.Text ->
      CF.Cofree (Q.Expr g T.Text) ann ->
      CF.Cofree (Q.Expr g T.Text) ann
    goName x xs e
      | S.size (S.delete x xs) < 1 = error "TODO explain internal error"
      | otherwise = foldr (goCopy x) e (S.delete x xs)

    goCopy ::
      T.Text ->
      T.Text ->
      CF.Cofree (Q.Expr g T.Text) ann ->
      CF.Cofree (Q.Expr g T.Text) ann
    goCopy x y e =
      ann
        CF.:< Q.Match
          (callFn ann nmCopy (ann CF.:< Q.Local x))
          (M.singleton (Just nmPair) ([x, y], e))
      where
        ann = extract e

freshNameLike :: (MonadState (S.Set T.Text) m) => T.Text -> m T.Text
freshNameLike x =
  gets (S.member x) >>= \case
    False -> modify (S.insert x) >> pure x
    True -> freshNameLike' 0
  where
    freshNameLike' (n :: Int) =
      let name = x <> T.pack (show n)
       in gets (S.member name) >>= \case
            False -> modify (S.insert name) >> pure name
            True -> freshNameLike' (succ n)

withoutLocals :: (MonadState (S.Set T.Text) m) => S.Set T.Text -> m a -> m a
withoutLocals xs ma = do
  xs' <- gets $ S.intersection xs
  modify (`S.difference` xs)
  a <- ma
  modify $ S.union xs'
  pure a
