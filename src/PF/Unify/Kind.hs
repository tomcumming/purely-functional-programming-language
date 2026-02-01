module PF.Unify.Kind
  ( unifyKnd,
    unifyKnd',
    occurs,
  )
where

import Control.Category ((>>>))
import Control.Monad (unless, when, (>=>))
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Free (Free (..))
import Control.Monad.Reader (runReaderT)
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
import Data.Set qualified as S
import PF.Unify.Data.Env (Env (..), Ext, ExtK, Knd, Lvl, Problem (..))
import PF.Unify.Data.Kind qualified as Knd
import PF.Unify.Subst (Subst)
import PF.Unify.Subst qualified as Subst

unifyKnd :: Env c -> Subst c -> Knd -> Knd -> Either Problem (Subst c)
unifyKnd env s k1 k2 =
  unifyKnd' k1 k2
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

occurs :: (Foldable f) => Ext s -> Free f (Ext s) -> Bool
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
