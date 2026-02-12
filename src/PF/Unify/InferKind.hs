module PF.Unify.InferKind (inferKnd) where

import Control.Category ((>>>))
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Free (Free (..))
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Control.Monad.State.Class (MonadState, gets, state)
import Control.Monad.Trans.Free qualified as TF
import Data.Function ((&))
import Data.Functor.Foldable (cata)
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Sq
import PF.Unify.Data.Env
  ( Env,
    Knd,
    Primitives (..),
    Problem (Unknown),
    Ty,
    currentLvl,
    envKnd,
    envPrims,
    envTy,
    pushVar,
    pattern (:->),
  )
import PF.Unify.Data.Kind qualified as Knd
import PF.Unify.Data.Type qualified as Ty
import PF.Unify.Kind (unifyKnd')
import PF.Unify.Subst (Subst, applyKnd, lookupKind, substKndLvl)

freshKnd ::
  (MonadState (Subst c) m) =>
  (MonadReader (Env c) m) =>
  m Knd
freshKnd = do
  lvl <- asks currentLvl
  state $ \s ->
    let x = substKndLvl s & M.lookupMax & maybe (toEnum 0) (fst >>> succ)
     in (Pure x, s {substKndLvl = M.insert x lvl (substKndLvl s)})

inferKnd ::
  (MonadState (Subst c) m) =>
  (MonadReader (Env c) m) =>
  (MonadError Problem m) =>
  (Ord c) =>
  Ty c ->
  m Knd
inferKnd = cata $ \case
  TF.Pure x -> gets (lookupKind x)
  TF.Free (Ty.Var i) ->
    asks (envKnd >>> (Sq.!? fromEnum i)) >>= \case
      Nothing -> error "Internal error: Type Var OOB"
      Just k -> pure k
  TF.Free (Ty.Con c) ->
    asks (envTy >>> (M.!? c)) >>= \case
      Nothing -> do
        Primitives {primRowNil, primRowCons} <- asks envPrims
        case () of
          () | c == primRowNil -> Free Knd.Row & pure
          ()
            | c == primRowCons ->
                Free Knd.Sym
                  :-> Free (Knd.Val (Free Knd.Sta))
                  :-> Free Knd.Row
                  :-> Free Knd.Row
                  & pure
          _ -> throwError Unknown
      Just k -> pure k
  TF.Free (Ty.For k mk) -> local (pushVar k) mk
  TF.Free (Ty.Ap mk1 mk2) -> do
    k1 <- mk1
    k2 <- mk2
    kr <-
      gets (applyKnd k1) >>= \case
        Free (Knd.Arr _ kr) -> pure kr
        _ -> freshKnd
    unifyKnd' k1 (Free (Knd.Arr k2 kr))
    pure kr
