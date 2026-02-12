module PF.Unify.Subst
  ( Subst (substKndLvl, substTy),
    applyKnd,
    applyTy,
    lookupKind,
    solveKnd,
    solveTy,
    pushUnsolvedTy,
  )
where

import Control.Category ((>>>))
import Control.Monad.Free (Free (..))
import Control.Monad.Trans.Free qualified as TF
import Data.Function ((&))
import Data.Functor.Foldable (cata)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Optics qualified as O
import PF.Unify.Data.Env (ExtK, ExtT, Knd, Lvl, Ty)

data Subst c = Subst
  { substKndLvl :: M.Map ExtK Lvl,
    substTy :: M.Map ExtT (Lvl, Knd),
    substSolvedKnd :: M.Map ExtK Knd,
    substSolvedTy :: M.Map ExtT (Ty c)
  }
  deriving (Generic, Show)

applyKnd :: Knd -> Subst c -> Knd
applyKnd = flip $ \Subst {substSolvedKnd} -> cata $ \case
  TF.Pure x -> substSolvedKnd M.!? x & fromMaybe (Pure x)
  TF.Free k -> Free k

applyTy :: Ty c -> Subst c -> Ty c
applyTy = flip $ \Subst {substSolvedTy} -> cata $ \case
  TF.Pure x -> substSolvedTy M.!? x & fromMaybe (Pure x)
  TF.Free t -> Free t

lookupKind :: ExtT -> Subst c -> Knd
lookupKind x s =
  substTy s M.!? x & \case
    Nothing -> error "Internal error: Unknown type ext"
    Just (_l, k) -> applyKnd k s

-- | This MUST be checked by caller
solveKnd :: ExtK -> Knd -> Subst c -> Subst c
solveKnd x k = O.over (O.gfield @"substSolvedKnd") (M.insert x k)

-- | This MUST be checked by caller
solveTy :: ExtT -> Ty c -> Subst c -> Subst c
solveTy x t = O.over (O.gfield @"substSolvedTy") (M.insert x t)

pushUnsolvedTy :: Lvl -> Knd -> Subst c -> (ExtT, Subst c)
pushUnsolvedTy l k s =
  let x = substTy s & M.lookupMax & maybe (toEnum 0) (fst >>> succ)
   in (x, O.over (O.gfield @"substTy") (M.insert x (l, k)) s)
