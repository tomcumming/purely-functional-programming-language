module PF.Check.Unify
  ( Problem (..),
    Env (..),
    pushVar,
    unifyKnd,
    unifyTy,
  )
where

import Control.Category ((>>>))
import Control.Monad (unless, when, (>=>))
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Free (Free (..))
import Control.Monad.Reader (local, runReaderT)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.State.Class (MonadState, gets, modify')
import Control.Monad.State.Strict (execStateT)
import Control.Monad.Trans.Free qualified as TF
import Data.Bifunctor (first)
import Data.Bitraversable (bitraverse, firstA)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Foldable (cata, embed, project)
import Data.Map.Strict qualified as M
import Data.Semigroup (Max (Max), getMax)
import Data.Sequence qualified as Sq
import Data.Set qualified as S
import PF.Check.Kind qualified as Knd
import PF.Check.Subst (ExtK, ExtT, Knd, Lvl, Subst, Ty)
import PF.Check.Subst qualified as Subst
import PF.Check.Type qualified as Ty

-- TODO wrappers each step for checking kind/type
data Problem
  = Mism
  | ForallEscape
  | Occurs
  deriving (Show)

data Env c = Env
  { envSkolTy :: S.Set ExtT,
    envSkolKnd :: S.Set ExtK,
    envKnd :: Sq.Seq Knd,
    envTy :: M.Map c Knd
  }
  deriving (Show)

pushVar :: Knd -> Env c -> Env c
pushVar k env = env {envKnd = k Sq.<| envKnd env}

unifyKnd :: Env c -> Subst c -> Knd -> Knd -> Either Problem (Subst c)
unifyKnd env s k1 k2 =
  unifyKnd' k1 k2
    & flip execStateT s
    & flip runReaderT env
    & runExcept

unifyTy :: (Eq c) => Env c -> Subst c -> Ty c -> Ty c -> Either Problem (Subst c)
unifyTy env s t1 t2 =
  unifyTy' t1 t2
    & flip execStateT s
    & flip runReaderT env
    & runExcept

unifyKnd' ::
  forall c m.
  (MonadState (Subst c) m) =>
  (MonadReader (Env c) m) =>
  (MonadError Problem m) =>
  Knd ->
  Knd ->
  m ()
unifyKnd' =
  curry $
    substBoth
      >=> bitraverse markSkol markSkol
      >=> \case
        (TF.Pure (x, _), TF.Pure (y, _)) | x == y -> pure ()
        (TF.Free Knd.Ty, TF.Free Knd.Ty) -> pure ()
        (TF.Free (Knd.Arr k11 k12), TF.Free (Knd.Arr k21 k22)) ->
          unifyKnd' k11 k21 >> unifyKnd' k12 k22
        (TF.Pure (x, False), k2) -> trySolveVar x k2
        (k1, TF.Pure (x, False)) -> trySolveVar x k1
        _ -> throwError Mism
  where
    substBoth (k1, k2) =
      (,) <$> gets (Subst.applyKnd k1) <*> gets (Subst.applyKnd k2)

    markSkol :: Knd -> m (TF.FreeF Knd.KindF (ExtK, Bool) Knd)
    markSkol =
      project
        >>> firstA (\x -> (x,) <$> asks (envSkolKnd >>> S.member x))

    trySolveVar :: ExtK -> TF.FreeF Knd.KindF (ExtK, Bool) Knd -> m ()
    trySolveVar x = \case
      TF.Pure (y, False) -> do
        xl <- kndExtLvl x
        yl <- kndExtLvl y
        modify' $
          if xl >= yl
            then Subst.solveKnd x (Pure y)
            else Subst.solveKnd y (Pure x)
      tfk -> do
        xl <- kndExtLvl x
        let k = first fst tfk & embed
        kndLvl k >>= \case
          Nothing -> pure ()
          Just kl -> unless (xl >= kl) (throwError ForallEscape)
        when (occurs x k) (throwError Occurs)
        Subst.solveKnd x k & modify'

unifyTy' ::
  forall c m.
  (MonadState (Subst c) m) =>
  (MonadReader (Env c) m) =>
  (MonadError Problem m) =>
  (Eq c) =>
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
        (TF.Free t1, TF.Free t2) -> unifyTys t1 t2
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
        xl <- tyExtLvl x
        yl <- tyExtLvl y
        modify' $
          if xl >= yl
            then Subst.solveTy x (Pure y)
            else Subst.solveTy y (Pure x)
      tfk -> do
        xl <- tyExtLvl x
        let k = first fst tfk & embed
        tyLvl k >>= \case
          Nothing -> pure ()
          Just kl -> unless (xl >= kl) (throwError ForallEscape)
        when (occurs x k) (throwError Occurs)
        Subst.solveTy x k & modify'

    unifyTys :: Ty.TyF Knd c (Ty c) -> Ty.TyF Knd c (Ty c) -> m ()
    unifyTys = curry $ \case
      (Ty.Var i1, Ty.Var i2) | i1 == i2 -> pure ()
      (Ty.Con c1, Ty.Con c2) | c1 == c2 -> pure ()
      (Ty.Ap t11 t12, Ty.Ap t21 t22) -> unifyTy' t11 t21 >> unifyTy' t12 t22
      (Ty.For k1 t1, Ty.For k2 t2) -> do
        unifyKnd' k1 k2
        local (pushVar k1) (unifyTy' t1 t2)
      _ -> throwError Mism

occurs :: (Foldable f) => Subst.Ext s -> Free f (Subst.Ext s) -> Bool
occurs = elem

kndExtLvl :: (MonadState (Subst c) m) => ExtK -> m Lvl
kndExtLvl x =
  gets (Subst.substKndLvl >>> (M.!? x)) >>= \case
    Nothing -> error "Internal error: Unknown kind level"
    Just l -> pure l

kndLvl ::
  (MonadState (Subst c) m) =>
  (MonadReader (Env c) m) =>
  Knd -> m (Maybe Lvl)
kndLvl = fmap (fmap (fmap getMax)) $ cata $ \case
  TF.Pure x -> kndExtLvl x <&> (Max >>> Just)
  TF.Free k -> sequenceA k <&> fold

tyExtLvl :: (MonadState (Subst c) m) => ExtT -> m Lvl
tyExtLvl x =
  gets (Subst.substTyLvl >>> (M.!? x)) >>= \case
    Nothing -> error "Internal error: Unknown ty level"
    Just l -> pure l

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
