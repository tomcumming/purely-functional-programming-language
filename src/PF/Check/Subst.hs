module PF.Check.Subst
  ( Lvl,
    Ext,
    ExtK,
    ExtT,
    Knd,
    Ty,
    Subst (substKndLvl, substTyLvl),
    applyKnd,
    applyTy,
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
import PF.Check.Kind qualified as Knd
import PF.Check.Type qualified as Ty

newtype Lvl = Lvl Int deriving newtype (Eq, Ord, Enum, Show)

newtype Ext s = TExt Int deriving newtype (Eq, Ord, Enum, Show)

type ExtK = Ext "K"

type ExtT = Ext "T"

type Knd = Free Knd.KindF ExtK

type Ty c = Free (Ty.TyF Knd c) ExtT

data Subst c = Subst
  { substKndLvl :: M.Map ExtK Lvl,
    substTyLvl :: M.Map ExtT Lvl,
    substKnd :: M.Map ExtK Knd,
    substTy :: M.Map ExtT (Ty c)
  }
  deriving (Show)

applyKnd :: Knd -> Subst c -> Knd
applyKnd = flip $ \Subst {substKnd} -> cata $ \case
  TF.Pure x -> substKnd M.!? x & fromMaybe (Pure x)
  TF.Free k -> Free k

applyTy :: Ty c -> Subst c -> Ty c
applyTy = flip $ \Subst {substTy} -> cata $ \case
  TF.Pure x -> substTy M.!? x & fromMaybe (Pure x)
  TF.Free t -> Free t

-- | This MUST be checked by caller
solveKnd :: ExtK -> Knd -> Subst c -> Subst c
solveKnd x k s =
  s
    { substKnd = M.insert x k (substKnd s)
    }

-- | This MUST be checked by caller
solveTy :: ExtT -> Ty c -> Subst c -> Subst c
solveTy x t s =
  s
    { substTy = M.insert x t (substTy s)
    }
