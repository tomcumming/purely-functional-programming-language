module PF.Unify.Data.Env
  ( Lvl,
    Ext,
    ExtK,
    ExtT,
    Knd,
    Ty,
    Problem (..),
    Env (..),
    pushVar,
    currentLvl,
  )
where

import Control.Category ((>>>))
import Control.Monad.Free (Free)
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Sq
import Data.Set qualified as S
import PF.Unify.Data.Kind qualified as Knd
import PF.Unify.Data.Type qualified as Ty

newtype Lvl = Lvl Int deriving newtype (Eq, Ord, Enum, Show)

newtype Ext s = TExt Int deriving newtype (Eq, Ord, Enum, Show)

type ExtK = Ext "K"

type ExtT = Ext "T"

type Knd = Free Knd.KindF ExtK

type Ty c = Free (Ty.TyF Knd c) ExtT

-- TODO wrappers each step for checking kind/type
data Problem
  = Mism
  | ForallEscape
  | Occurs
  | Unknown
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

currentLvl :: Env c -> Lvl
currentLvl = envKnd >>> Sq.length >>> toEnum
