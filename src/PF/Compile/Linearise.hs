module PF.Compile.Linearise
  ( Names (..),
    LinearExpr,
    getLinearExpr,
    linearise,
  )
where

import Control.Category ((>>>))
import Control.Comonad (extract)
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CFF
import Control.Monad (forM, replicateM, (>=>))
import Control.Monad.Reader (runReader, runReaderT)
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Control.Monad.State (evalState)
import Control.Monad.State.Class (MonadState, state)
import Data.Bifunctor (second)
import Data.Functor.Foldable (cata)
import Data.Map qualified as M
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import PF.Expr.QualNamed qualified as N
import PF.Expr.Qualified qualified as Q

data Names g = Names
  { nameUnit :: g,
    nameTuple :: g,
    nameDrop :: g,
    nameCopy :: g
  }

type QExpr g ann = CF.Cofree (Q.Expr g) ann

type NExpr g ann = CF.Cofree (N.Expr Unique g) ann

newtype LinearExpr g ann = LinearExpr
  {getLinearExpr :: QExpr g ann}

newtype Unique = Unique Word
  deriving newtype (Eq, Ord, Enum)

linearise :: Names g -> QExpr g ann -> LinearExpr g ann
linearise names =
  (intoQualNamed >=> dropsAndCopies names)
    >>> flip evalState (Unique 0)
    >>> snd
    >>> fromQualNamed
    >>> LinearExpr

dropsAndCopies ::
  forall g ann m.
  (MonadState Unique m) =>
  Names g ->
  NExpr g ann ->
  m (M.Map Unique ann, NExpr g ann)
dropsAndCopies Names {..} = cata $ \(ann CFF.:< inExpr) -> case inExpr of
  N.Local l -> pure (M.singleton l ann, ann CF.:< N.Local l)
  N.Global g -> pure (mempty, ann CF.:< N.Global g)
  N.Abs l me -> do
    (us, e) <- me
    pure (M.delete l us, ann CF.:< N.Abs l (doDrop ann us l e))
  N.Ap me1 me2 -> do
    (us1, e1) <- me1
    (us2, e2) <- me2
    let is = M.intersection us1 us2
    (cpy, s) <- doCopy is
    pure (us1 <> us2, cpy $ ann CF.:< N.Ap e1 (N.subst s e2))
  N.Match me1 mbs -> do
    (us1, e1) <- me1
    bs <- forM mbs $ \(ls, me) -> do
      (us, e) <- me
      pure (us `M.withoutKeys` S.fromList ls, (ls, doDrops ann us ls e))
    let ubs = foldMap fst bs -- used in any branch
    let bs' = fmap (branchDrop ubs) bs
    let is = M.intersection us1 ubs
    (cpy, s) <- doCopy is
    let bs'' = fmap (second (N.subst s)) bs'
    pure (us1 <> ubs, cpy $ ann CF.:< N.Match e1 bs'')
  where
    doDrop :: ann -> M.Map Unique ann -> Unique -> NExpr g ann -> NExpr g ann
    doDrop ann us l
      | M.member l us = id
      | otherwise = \e ->
          ann
            CF.:< N.Match
              (mkCall ann nameDrop $ ann CF.:< N.Local l)
              (M.singleton (Just nameUnit) ([], e))

    doDrops ::
      ann ->
      M.Map Unique ann ->
      [Unique] ->
      NExpr g ann ->
      NExpr g ann
    doDrops ann us ls e = foldr (doDrop ann us) e ls

    doCopy ::
      M.Map Unique ann ->
      m (NExpr g ann -> NExpr g ann, M.Map Unique (NExpr g ann))
    doCopy = M.foldrWithKey go (pure (id, mempty))
      where
        go l ann mfs = do
          (f, s) <- mfs
          l' <- fresh
          let f' e =
                ann
                  CF.:< N.Match
                    (mkCall ann nameCopy $ ann CF.:< N.Local l)
                    (M.singleton (Just nameTuple) ([l, l'], e))
          pure (f' . f, M.insert l (ann CF.:< N.Local l') s)

    branchDrop ::
      M.Map Unique ann ->
      (M.Map Unique ann, ([Unique], NExpr g ann)) ->
      ([Unique], NExpr g ann)
    branchDrop ubs (us, (ls, e)) = (ls, doDrops (extract e) us (M.keys ubs) e)

mkCall ::
  ann ->
  g ->
  CF.Cofree (N.Expr Unique g) ann ->
  CF.Cofree (N.Expr Unique g) ann
mkCall ann g e = ann CF.:< N.Ap (ann CF.:< N.Global g) e

intoQualNamed ::
  forall m g ann.
  (MonadState Unique m) =>
  CF.Cofree (Q.Expr g) ann ->
  m (CF.Cofree (N.Expr Unique g) ann)
intoQualNamed = cata go >>> flip runReaderT mempty
  where
    go ::
      (MonadReader (Seq.Seq Unique) m') =>
      (MonadState Unique m') =>
      CFF.CofreeF
        (Q.Expr g)
        ann
        (m' (CF.Cofree (N.Expr Unique g) ann)) ->
      m' (CF.Cofree (N.Expr Unique g) ann)
    go (ann CFF.:< inExpr) = case inExpr of
      Q.Local i ->
        asks (Seq.!? fromEnum i) >>= \case
          Nothing -> error "OOB index!"
          Just u -> pure $ ann CF.:< N.Local u
      Q.Global g -> pure $ ann CF.:< N.Global g
      Q.Abs meBody -> do
        u <- fresh
        eBody <- local (u Seq.<|) meBody
        pure $ ann CF.:< N.Abs u eBody
      Q.Ap me1 me2 -> (ann CF.:<) <$> (N.Ap <$> me1 <*> me2)
      Q.Match me1 mbs -> do
        e1 <- me1
        bs <- forM mbs $ \(n, me) -> do
          ls <- replicateM (fromIntegral n) fresh
          e <- local (Seq.fromList ls <>) me
          pure (ls, e)
        pure $ ann CF.:< N.Match e1 bs

fromQualNamed :: CF.Cofree (N.Expr Unique g) ann -> CF.Cofree (Q.Expr g) ann
fromQualNamed = cata go >>> flip runReader mempty
  where
    go ::
      (MonadReader (Seq.Seq Unique) m) =>
      CFF.CofreeF (N.Expr Unique g) ann (m (CF.Cofree (Q.Expr g) ann)) ->
      m (CF.Cofree (Q.Expr g) ann)
    go (ann CFF.:< inExpr) = case inExpr of
      N.Local u ->
        asks (Seq.findIndexL (== u)) >>= \case
          Nothing -> error "Unknown unique ID!"
          Just i -> pure $ ann CF.:< Q.Local (toEnum i)
      N.Global g -> pure $ ann CF.:< Q.Global g
      N.Abs u meBody -> do
        eBody <- local (u Seq.<|) meBody
        pure $ ann CF.:< Q.Abs eBody
      N.Ap me1 me2 -> (ann CF.:<) <$> (Q.Ap <$> me1 <*> me2)
      N.Match me1 mbs -> do
        e1 <- me1
        bs <- forM mbs $ \(ls, me) -> do
          e <- local (Seq.fromList ls <>) me
          pure (fromIntegral $ length ls, e)
        pure $ ann CF.:< Q.Match e1 bs

fresh :: (MonadState Unique m') => m' Unique
fresh = state $ \u -> (u, succ u)
