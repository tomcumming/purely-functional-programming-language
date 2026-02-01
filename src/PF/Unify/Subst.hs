module PF.Unify.Subst
  ( Subst (substKndLvl, substTyLvl),
    applyKnd,
    applyTy,
    lookupKind,
    solveKnd,
    solveTy,
  )
where

import Control.Monad.Free (Free (..))
import Control.Monad.Trans.Free qualified as TF
import Data.Function ((&))
import Data.Functor.Foldable (cata)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import PF.Unify.Data.Env (ExtK, ExtT, Knd, Lvl, Ty)

data Subst c = Subst
  { substKndLvl :: M.Map ExtK Lvl,
    substTyLvl :: M.Map ExtT Lvl,
    substTyKnd :: M.Map ExtT Knd,
    substSolvedKnd :: M.Map ExtK Knd,
    substSolvedTy :: M.Map ExtT (Ty c)
  }
  deriving (Show)

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
  substTyKnd s M.!? x & \case
    Nothing -> error "Internal error: Unknown type ext"
    Just k -> applyKnd k s

-- | This MUST be checked by caller
solveKnd :: ExtK -> Knd -> Subst c -> Subst c
solveKnd x k s =
  s
    { substSolvedKnd = M.insert x k (substSolvedKnd s)
    }

-- | This MUST be checked by caller
solveTy :: ExtT -> Ty c -> Subst c -> Subst c
solveTy x t s =
  s
    { substSolvedTy = M.insert x t (substSolvedTy s)
    }
