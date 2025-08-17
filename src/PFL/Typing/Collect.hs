module PFL.Typing.Collect
  ( Globals (..),
    Collected (..),
    Issue (..),
    Constraint (..),
    collect,
  )
where

import Control.Comonad (extract)
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CFT
import Control.Monad.Free (Free (..))
import Control.Monad.RWS (evalRWS)
import Control.Monad.RWS.Class (MonadRWS, asks, local, state, tell)
import Data.Functor.Foldable (cataA)
import Data.Map qualified as M
import Data.Set qualified as S
import PFL.Expr.LambdaLifted qualified as LL
import PFL.Typing.Kind (Kind)
import PFL.Typing.Kind qualified as Kind
import PFL.Typing.Type qualified as Ty

data VarType = TyVar | KindVar | InstVar
  deriving (Show)

newtype Ext (vt :: VarType) = Ext Word
  deriving newtype (Eq, Ord, Enum, Show)

type TyVar' = Ext TyVar

type KindVar' = Ext KindVar

type Kind' = Either KindVar' Kind

type Ty' g = Free (Ty.Ty Kind' g) TyVar'

data Globals g g' = Globals
  { gblUnknown :: g',
    gblClosure :: g',
    gblPair :: g',
    gblConstrs :: M.Map g (Ty' g', [Ty' g']), -- These should actually be some solved type
    gblNamed :: M.Map g (g', Ty' g') -- as should these
  }

data Env g g' l ann = Env
  { envLocals :: M.Map l (Ty' g'),
    envGlobals :: Globals g g',
    envAnns :: S.Set ann
  }

-- | Collect State
data St = St
  { stFreshType :: TyVar',
    stFreshKind :: KindVar'
  }

data Issue g l
  = UnknownLocal l TyVar'
  | UnknownGlobal g TyVar'
  deriving (Eq, Ord, Show)

data Constraint g'
  = TyKind (Ty' g') Kind'
  | TyEq (Ty' g')
  | KindEq Kind' Kind'
  deriving (Eq, Ord, Show)

data Collected g g' l ann = Collected
  { colIssues :: M.Map (Issue g l) (S.Set ann),
    colConstraints :: M.Map (Constraint g') (S.Set ann)
  }

instance (Ord l, Ord g, Ord g', Ord ann) => Semigroup (Collected g g' l ann) where
  c1 <> c2 =
    Collected
      { colIssues =
          M.unionWith
            (<>)
            (colIssues c1)
            (colIssues c2),
        colConstraints =
          M.unionWith
            (<>)
            (colConstraints c1)
            (colConstraints c2)
      }

instance (Ord l, Ord g, Ord g', Ord ann) => Monoid (Collected g g' l ann) where
  mempty =
    Collected
      { colIssues = mempty,
        colConstraints = mempty
      }

type LExpr g l ann = CF.Cofree (LL.Expr g l) ann

type AExpr g' l ann = CF.Cofree (LL.Expr g' l) (Ty' g', ann)

type Collect g g' l ann m =
  ( MonadRWS (Env g g' l ann) (Collected g g' l ann) St m,
    Ord l,
    Ord g,
    Ord g'
  )

emptyEnv :: (Ord l, Ord ann) => Globals g g' -> Env g g' l ann
emptyEnv envGlobals = Env {envGlobals, envLocals = mempty, envAnns = mempty}

emptyState :: St
emptyState =
  St
    { stFreshType = Ext 0,
      stFreshKind = Ext 0
    }

collect ::
  (Ord ann, Ord l, Ord g, Ord g') =>
  Globals g g' ->
  LExpr g l ann ->
  (AExpr g' l ann, Collected g g' l ann)
collect gbs e = evalRWS (collect' e) (emptyEnv gbs) emptyState

collect' :: (Collect g g' l ann m) => LExpr g l ann -> m (AExpr g' l ann)
collect' = cataA $ \case
  ann CFT.:< LL.Local x ->
    withAnns (S.singleton ann) $
      asks (M.lookup x . envLocals) >>= \case
        Nothing -> do
          a <- freshType $ Right Kind.Type
          tellIssue $ UnknownLocal x a
          pure $ (Pure a, ann) CF.:< LL.Local x
        Just t -> do
          tellCons $ TyKind t (Right Kind.Type)
          pure $ (t, ann) CF.:< LL.Local x
  ann CFT.:< LL.Global x ->
    withAnns (S.singleton ann) $
      asks (M.lookup x . gblNamed . envGlobals) >>= \case
        Nothing -> do
          a <- freshType $ Right Kind.Type
          tellIssue $ UnknownGlobal x a
          x' <- asks (gblUnknown . envGlobals)
          pure $ (Pure a, ann) CF.:< LL.Global x'
        Just (x', t) -> do
          -- TODO we need to instantiate the type here, it will be a scheme
          tellCons $ TyKind t (Right Kind.Type)
          pure $ (t, ann) CF.:< LL.Global x'
  ann CFT.:< LL.Closure me1 x me2 -> do
    eCtx <- me1
    let tCtx = fst $ extract eCtx
    tArg <- Pure <$> freshType (Right Kind.Type)
    tCtxArg <- pairUp tCtx tArg
    eRet <- pushLocal x tCtxArg me2
    let tRet = fst $ extract eRet
    t <- typeClosure tCtx tArg tRet
    pure $ (t, ann) CF.:< LL.Closure eCtx x eRet

pushLocal :: (Collect g g' l ann m) => l -> Ty' g' -> m a -> m a
pushLocal x t = local $ \env ->
  env {envLocals = M.insert x t (envLocals env)}

withAnns :: (Collect g g' l ann m) => S.Set ann -> m a -> m a
withAnns envAnns = local (\env -> env {envAnns})

tellCons :: forall g g' l ann m. (Collect g g' l ann m) => Constraint g' -> m ()
tellCons c = do
  anns <- asks envAnns
  -- We should not need to annotate mempty...
  tell $ (mempty :: Collected g g' l ann) {colConstraints = M.singleton c anns}

tellIssue :: forall g g' l ann m. (Collect g g' l ann m) => Issue g l -> m ()
tellIssue i = do
  anns <- asks envAnns
  tell $ (mempty :: Collected g g' l ann) {colIssues = M.singleton i anns}

freshType :: (Collect g g' l ann m) => Kind' -> m TyVar'
freshType k = do
  a <- state $ \st ->
    ( stFreshType st,
      st {stFreshType = succ (stFreshType st)}
    )
  tellCons $ TyKind (Pure a) k
  pure a

pairUp :: (Collect g g' l ann m) => Ty' g' -> Ty' g' -> m (Ty' g')
pairUp t1 t2 = do
  gPair <- asks $ gblPair . envGlobals
  pure $
    Free $
      Ty.Ap
        (Free $ Ty.Ap (Free $ Ty.Var gPair) t1)
        t2

typeClosure :: (Collect g g' l ann m) => Ty' g' -> Ty' g' -> Ty' g' -> m (Ty' g')
typeClosure tCtx tArg tRet = do
  gClosure <- asks $ gblClosure . envGlobals
  pure $
    Free $
      Ty.Ap
        ( Free $
            Ty.Ap
              (Free $ Ty.Ap (Free $ Ty.Var gClosure) tCtx)
              tArg
        )
        tRet
