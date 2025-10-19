module PF.Check.Unify
  ( Var,
    Typ,
    Subst,
    subst,
    unify,
    match,
  )
where

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Except (runExcept)
import Control.Monad.Free (Free (..))
import Control.Monad.State (execStateT)
import Control.Monad.State.Class (MonadState, get, modify)
import Control.Monad.Trans.Free qualified as FF
import Data.Function ((&))
import Data.Functor.Foldable (cata)
import Data.Map qualified as M
import PF.Check.Type qualified as Ty

{- TODO
- We will have to handle levels of type vars, probably adding to state somehow,
  so we can substitute the right direction. (Forall escape)
- Unify error description
-}

newtype Var = Var Int
  deriving newtype (Eq, Ord, Enum, Show)

type Typ c = Free (Ty.Typ c) Var

type UTyp c = Free (Ty.Typ c) Skol

data Skol
  = SV Var
  | Skol Var
  deriving (Eq, Ord, Show)

liftSkol :: Typ c -> UTyp c
liftSkol = fmap SV

skolemise :: Typ c -> UTyp c
skolemise = fmap Skol

type Subst c = M.Map Var (Typ c)

type USubst c = M.Map Var (UTyp c)

unskolSubst :: USubst c -> Subst c
unskolSubst = fmap $ fmap unskol
  where
    unskol = \case
      SV x -> x
      Skol x -> x

subst :: Subst c -> Typ c -> Typ c
subst s = cata $ \case
  FF.Pure x
    | Just t <- s M.!? x -> t
    | otherwise -> Pure x
  FF.Free t -> Free t

usubst :: USubst c -> UTyp c -> UTyp c
usubst s = cata $ \case
  FF.Pure sv
    | SV x <- sv,
      Just t <- s M.!? x ->
        t
    | otherwise -> Pure sv
  FF.Free t -> Free t

addSubst :: Var -> UTyp c -> USubst c -> USubst c
addSubst x t = fmap (usubst $ M.singleton x t) >>> M.insert x t

occurs :: Var -> UTyp c -> Bool
occurs x = cata $ \case
  FF.Pure (SV y) -> x == y
  t -> or t

unify :: (Ord c) => Typ c -> Typ c -> Either () (Subst c)
unify t1 t2 =
  unify' (liftSkol t1) (liftSkol t2)
    & flip execStateT mempty
    & runExcept
    & fmap unskolSubst

-- | Find the substitution that turns lhs into rhs
match :: (Ord c) => Typ c -> Typ c -> Either () (Subst c)
match t1 t2 =
  unify' (liftSkol t1) (skolemise t2)
    & flip execStateT mempty
    & runExcept
    & fmap unskolSubst

unify' ::
  forall c m.
  (MonadState (USubst c) m) =>
  (MonadError () m) =>
  (Eq c) =>
  UTyp c ->
  UTyp c ->
  m ()
unify' =
  curry $
    substBoth >=> \case
      (Pure (SV x), t) -> goVar x t
      (t, Pure (SV x)) -> goVar x t
      (Pure (Skol s1), Pure (Skol s2)) | s1 == s2 -> pure ()
      (Free t1, Free t2) -> goTy t1 t2
      _ -> throwError ()
  where
    substBoth (t1, t2) = get >>= \s -> pure (usubst s t1, usubst s t2)

    goVar :: Var -> UTyp c -> m ()
    goVar x = \case
      Pure sv
        | SV x == sv -> pure ()
        | otherwise -> throwError ()
      Free t
        | occurs x (Free t) -> throwError ()
        | otherwise -> modify $ addSubst x (Free t)

    goTy :: Ty.Typ c (UTyp c) -> Ty.Typ c (UTyp c) -> m ()
    goTy = curry $ \case
      (Ty.Con c1, Ty.Con c2) | c1 == c2 -> pure ()
      (Ty.Forall k1 t1, Ty.Forall k2 t2) -> unify' k1 k2 >> unify' t1 t2
      (Ty.Ap t11 t12, Ty.Ap t21 t22) -> unify' t11 t21 >> unify' t12 t22
      _ -> throwError ()
