module PFL.Compile.Linearise
  ( linearise,
    Uid (..),
    Unique (..),
    Linearised,
    unLinearised,
  )
where

import Control.Category ((>>>))
import Control.Comonad (extract)
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CFT
import Control.Monad.State (MonadState, evalState, state)
import Data.Bifunctor (first)
import Data.Foldable (foldrM)
import Data.Functor.Foldable (cataA)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import PFL.Expr.Qualified qualified as Q

newtype Uid = Uid Int
  deriving newtype (Eq, Ord, Enum, Show)

data Unique = Unique !T.Text !Uid
  deriving (Eq, Ord, Show)

newtype Linearised ann = Linearised {unLinearised :: LExpr ann}

type QExpr ann = CF.Cofree (Q.Expr T.Text T.Text) ann

type LExpr ann = CF.Cofree (Q.Expr T.Text Unique) ann

-- TODO we need to catch and report on patterns with duplicate names:
--   \(a, a) -> ...

-- | Add drops and copies so every introduced name in the output expression is
-- used linearly.
linearise :: forall ann. QExpr ann -> Linearised ann
linearise = \e -> Linearised $ snd $ evalState (cataA go e) (toEnum 0 :: Uid)
  where
    fresh :: (MonadState Uid m) => m Uid
    fresh = state $ \n -> (n, succ n)

    go ::
      forall m ret.
      (MonadState Uid m, ret ~ (M.Map T.Text Uid, LExpr ann)) =>
      CFT.CofreeF (Q.Expr T.Text T.Text) ann (m ret) -> m ret
    go = \case
      (ann CFT.:< Q.Local x) -> do
        uid <- fresh
        let x' = Unique x uid
        pure (M.singleton x uid, ann CF.:< Q.Local x')
      (ann CFT.:< Q.Global x) -> pure (mempty, ann CF.:< Q.Global x)
      (ann CFT.:< Q.Abs x me) -> do
        (x', (used, e')) <- goIntro ann x =<< me
        pure (used, ann CF.:< Q.Abs x' e')
      (ann CFT.:< Q.Ap me1 me2) -> do
        (us1, e1) <- me1
        (us2, e2) <- me2
        let f = doCopy ann [us1, us2]
        pure $ f $ ann CF.:< Q.Ap e1 e2
      (ann CFT.:< Q.Match me mbs) -> do
        (ue, e) <- me
        (ubs, bs) <- do
          bs' <- traverse (uncurry goMatchBranch) mbs
          pure (fst <$> M.elems bs', snd <$> bs')
        let f = doCopy ann (ue : ubs)
        pure $ f $ ann CF.:< Q.Match e bs
      where
        goIntro :: ann -> T.Text -> ret -> m (Unique, ret)
        goIntro ann x (used, e) = do
          let (x', used', e') = doDrop ann x used e
          pure (x', (used', e'))

        goMatchBranch ::
          [T.Text] ->
          m (M.Map T.Text Uid, LExpr ann) ->
          m (M.Map T.Text Uid, ([Unique], LExpr ann))
        goMatchBranch xs me = do
          e <- me
          (us, (used, e')) <-
            foldrM
              (\x (us, e') -> first (: us) <$> goIntro (extract $ snd e) x e')
              ([], e)
              xs
          pure (used, (us, e'))

doDrop ::
  ann ->
  T.Text ->
  M.Map T.Text Uid ->
  LExpr ann ->
  (Unique, M.Map T.Text Uid, LExpr ann)
doDrop ann x used e = case used M.!? x of
  Nothing ->
    ( x',
      M.delete x used,
      ann
        CF.:< Q.Match
          (callDrop ann $ ann CF.:< Q.Local x')
          (M.singleton (Just "Unit") ([], e))
    )
    where
      x' = Unique x (toEnum 0)
  Just u ->
    ( Unique x u,
      M.delete x used,
      e
    )

doCopy ::
  forall ann.
  ann ->
  [M.Map T.Text Uid] ->
  LExpr ann ->
  (M.Map T.Text Uid, LExpr ann)
doCopy ann us =
  let (ds, ls) = takeDupes us
   in \e -> M.foldrWithKey goName (ls, e) ds
  where
    goName ::
      T.Text ->
      (Uid, NE.NonEmpty Uid) ->
      (M.Map T.Text Uid, LExpr ann) ->
      (M.Map T.Text Uid, LExpr ann)
    goName x (u, us') (used, e) = foldr (goDup (Unique x u)) (used, e) us'

    goDup ::
      Unique ->
      Uid ->
      (M.Map T.Text Uid, LExpr ann) ->
      (M.Map T.Text Uid, LExpr ann)
    goDup x@(Unique xx xu) u (used, e) =
      ( M.insert xx xu used,
        ann
          CF.:< Q.Match
            (callCopy ann $ ann CF.:< Q.Local x)
            (M.singleton (Just "Pair") ([x, Unique xx u], e))
      )

callDrop :: ann -> LExpr ann -> LExpr ann
callDrop ann = (ann CF.:<) . Q.Ap (ann CF.:< Q.Global "drop")

callCopy :: ann -> LExpr ann -> LExpr ann
callCopy ann = (ann CF.:<) . Q.Ap (ann CF.:< Q.Global "copy")

takeDupes ::
  [M.Map T.Text Uid] ->
  ( M.Map T.Text (Uid, NE.NonEmpty Uid),
    M.Map T.Text Uid
  )
takeDupes ins =
  ( M.mapMaybe (either (const Nothing) Just) parted,
    M.mapMaybe (either Just (const Nothing)) parted
  )
  where
    parted =
      M.mapMaybe oneOrMany $
        M.unionsWith (<>) $
          fmap (fmap S.singleton) ins

    oneOrMany =
      S.toList >>> \case
        [] -> Nothing
        [x] -> Just $ Left x
        (x : y : zs) -> Just $ Right (x, y NE.:| zs)
