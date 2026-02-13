module PF.Unify.Type (unifyTy, unifyTy', freshTy) where

import Control.Category ((>>>))
import Control.Monad (join, unless, when, (>=>))
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Free (Free (..))
import Control.Monad.Reader (local, runReaderT)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.State.Class (MonadState, gets, modify', state)
import Control.Monad.State.Strict (execStateT)
import Control.Monad.Trans.Free qualified as TF
import Data.Bifunctor (bimap, first)
import Data.Bitraversable (bitraverse, firstA)
import Data.Foldable (fold, traverse_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Foldable (cata, embed, project)
import Data.Map.Strict qualified as M
import Data.Semigroup (Max (Max), getMax)
import Data.Sequence qualified as Sq
import Data.Set qualified as S
import Optics qualified as O
import PF.Data.Row qualified as Row
import PF.Unify.Data.Env
  ( Env (..),
    ExtT,
    Knd,
    Lvl,
    Primitives (..),
    Problem (..),
    Ty,
    currentLvl,
    pushVar,
    pattern Cn,
    pattern (:$),
  )
import PF.Unify.Data.Kind qualified as Knd
import PF.Unify.Data.Type qualified as Ty
import PF.Unify.InferKind (inferKnd)
import PF.Unify.Kind (occurs, unifyKnd')
import PF.Unify.Subst (Subst)
import PF.Unify.Subst qualified as Subst

unifyTy :: (Ord c) => Env c -> Subst c -> Ty c -> Ty c -> Either Problem (Subst c)
unifyTy env s t1 t2 =
  unifyTy' t1 t2
    & flip execStateT s
    & flip runReaderT env
    & runExcept

unifyTy' ::
  forall c m.
  (MonadState (Subst c) m) =>
  (MonadReader (Env c) m) =>
  (MonadError Problem m) =>
  (Ord c) =>
  Ty c ->
  Ty c ->
  m ()
unifyTy' =
  curry $
    substBoth
      >=> bitraverse markSkol markSkol
      >=> \case
        (TF.Pure (x, _), TF.Pure (y, _)) | x == y -> pure ()
        (TF.Pure (x, False), k2) -> trySolveVar x k2
        (k1, TF.Pure (x, False)) -> trySolveVar x k1
        (TF.Free t1, TF.Free t2) -> do
          prims <- asks envPrims
          unifyTys prims t1 t2
        _ -> throwError Mism
  where
    substBoth (t1, t2) =
      (,) <$> gets (Subst.applyTy t1) <*> gets (Subst.applyTy t2)

    markSkol :: Ty c -> m (TF.FreeF (Ty.TyF Knd c) (ExtT, Bool) (Ty c))
    markSkol =
      project
        >>> firstA (\x -> (x,) <$> asks (envSkolTy >>> S.member x))

    trySolveVar :: ExtT -> TF.FreeF (Ty.TyF Knd c) (ExtT, Bool) (Ty c) -> m ()
    trySolveVar x = \case
      TF.Pure (y, False) -> do
        unifyTyKnd (Pure x) (Pure y)
        xl <- tyExtLvl x
        yl <- tyExtLvl y
        modify' $
          if xl >= yl
            then Subst.solveTy x (Pure y)
            else Subst.solveTy y (Pure x)
      tft -> do
        xl <- tyExtLvl x
        let t = first fst tft & embed
        tyLvl t >>= \case
          Nothing -> pure ()
          Just kl -> unless (xl >= kl) (throwError ForallEscape)
        unifyTyKnd (Pure x) t
        when (occurs x t) (throwError Occurs)
        -- todo unify kinds
        Subst.solveTy x t & modify'

    unifyTys ::
      Primitives c ->
      Ty.TyF Knd c (Ty c) ->
      Ty.TyF Knd c (Ty c) ->
      m ()
    unifyTys prims = curry $ \case
      (Ty.Var i1, Ty.Var i2) | i1 == i2 -> pure ()
      (Ty.Con c1, Ty.Con c2) | c1 == c2 -> pure ()
      (t1, t2)
        | Just {} <- O.preview (rowCons prims) (Free t1),
          Just {} <- O.preview (rowCons prims) (Free t2) ->
            unifyRows prims (Free t1) (Free t2)
      (Ty.Ap t11 t12, Ty.Ap t21 t22) -> unifyTy' t11 t21 >> unifyTy' t12 t22
      (Ty.For k1 t1, Ty.For k2 t2) -> do
        unifyKnd' k1 k2
        local (pushVar k1) (unifyTy' t1 t2)
      _ -> throwError Mism

    unifyRows :: Primitives c -> Ty c -> Ty c -> m ()
    unifyRows prims =
      curry $
        bimap (intoRow prims) (intoRow prims)
          >>> uncurry Row.zipRows
          >>> \case
            (es1, es2, zs) -> do
              -- Unify matching keys
              traverse_ (uncurry unifyTy') (O.view Row.kvs zs)
              -- Unify tails
              let (z1, z2) = O.view Row.rtail zs
              z3 <- freshTy (Free Knd.Row) <&> Pure
              unifyTy' z1 (Row.make es2 z3 & fromRow prims)
              unifyTy' z2 (Row.make es1 z3 & fromRow prims)

unifyTyKnd ::
  (MonadError Problem m) =>
  (MonadState (Subst c) m) =>
  (MonadReader (Env c) m) =>
  (Ord c) =>
  Ty c -> Ty c -> m ()
unifyTyKnd t1 t2 = unifyKnd' <$> inferKnd t1 <*> inferKnd t2 & join

tyExtLvl :: (MonadState (Subst c) m) => ExtT -> m Lvl
tyExtLvl x =
  gets (Subst.substTy >>> (M.!? x)) >>= \case
    Nothing -> error "Internal error: Unknown ty level"
    Just (l, _k) -> pure l

tyLvl ::
  (MonadState (Subst c) m) =>
  (MonadReader (Env c) m) =>
  Ty c -> m (Maybe Lvl)
tyLvl = fmap (fmap (fmap getMax)) $ cata $ \case
  TF.Pure x -> tyExtLvl x <&> (Max >>> Just)
  TF.Free (Ty.Var i) -> do
    deptch <- asks (envKnd >>> Sq.length)
    deptch - fromEnum i & toEnum & Just & pure
  TF.Free k -> sequenceA k <&> fold

freshTy ::
  (MonadState (Subst c) m) =>
  (MonadReader (Env c) m) =>
  Knd -> m ExtT
freshTy k = do
  lvl <- asks currentLvl
  Subst.pushUnsolvedTy lvl k & state

rowCons :: (Eq c) => Primitives c -> O.Prism' (Ty c) ((Ty c, Ty c), Ty c)
rowCons Primitives {primRowCons} =
  O.prism'
    (\((tl, tv), tt) -> ((Cn primRowCons :$ tl) :$ tv) :$ tt)
    ( \case
        (((Cn c :$ k) :$ v) :$ t) | c == primRowCons -> Just ((k, v), t)
        _ -> Nothing
    )

fromRow :: (Ord c) => Primitives c -> Row.Row (Ty c) (Ty c) (Ty c) -> Ty c
fromRow prims =
  Row.popMin >>> \case
    Left tt -> tt
    Right ((tl, tv), r) -> O.review (rowCons prims) ((tl, tv), fromRow prims r)

intoRow :: (Ord c) => Primitives c -> Ty c -> Row.Row (Ty c) (Ty c) (Ty c)
intoRow prims t = case O.preview (rowCons prims) t of
  Nothing -> Row.empty t
  Just ((tl, tv), tt) -> intoRow prims tt & Row.push tl tv
