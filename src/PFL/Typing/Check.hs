module PFL.Typing.Check
  ( St (..),
    Env (..),
    Issue (..),
    Constraint (..),
    infer,
  )
where

import Control.Comonad (extract)
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CFT
import Control.Monad (join, zipWithM)
import Control.Monad.Free (Free (..))
import Control.Monad.RWS.Class (MonadRWS, asks, local, modify, state, tell)
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

data Constraint g
  = TyEq (Ty' g) (Ty' g)
  | KindEq Kind' Kind'
  | TyKind (Ty' g) Kind'
  | IsKind KindVar'
  deriving (Eq, Ord, Show)

-- | Solver State
data St g = St
  { -- | Type substitution
    stTy :: M.Map TyVar' (Ty' g),
    -- | Kind substitution
    stKind :: M.Map KindVar' Kind',
    -- | Unsolved constraints
    stCons :: S.Set (Constraint g),
    stFreshKind :: KindVar',
    stFreshTy :: TyVar'
  }

data Issue g l
  = UnknownLocal l TyVar'
  | UnknownGlobal g TyVar'
  deriving (Show)

data Env g l = Env
  { envClosure :: g,
    envPair :: g,
    envConstrs :: M.Map g (Ty' g, [Ty' g]), -- These should actually be some solved type
    envLocals :: M.Map l (Ty' g),
    envGlobals :: M.Map g (Ty' g) -- as should these
  }

type LExpr g l ann = CF.Cofree (LL.Expr g l) ann

type AExpr g l ann = CF.Cofree (LL.Expr g l) (Ty' g, ann)

type Check g l m =
  ( MonadRWS (Env g l) (S.Set (Issue g l)) (St g) m,
    Ord l,
    Ord g
  )

pushCons :: (Check g l m) => Constraint g -> m ()
pushCons c = do
  modify $ \st -> st {stCons = S.insert c (stCons st)}

-- TODO solve as much as possible

pushLocal :: (Check g l m) => l -> Ty' g -> m a -> m a
pushLocal x t = local $ \env ->
  env
    { envLocals = M.insert x t (envLocals env)
    }

infer ::
  forall g l ann m.
  (Check g l m) =>
  LExpr g l ann ->
  m (AExpr g l ann)
infer = cataA $ \case
  ann CFT.:< LL.Local x ->
    asks ((M.!? x) . envLocals) >>= \case
      Nothing -> do
        a <- unknownName
        tell $ S.singleton $ UnknownLocal x a
        pure $ (Pure a, ann) CF.:< LL.Local x
      Just t -> pure $ (t, ann) CF.:< LL.Local x
  ann CFT.:< LL.Global x ->
    asks ((M.!? x) . envGlobals) >>= \case
      Nothing -> do
        a <- unknownName
        tell $ S.singleton $ UnknownGlobal x a
        pure $ (Pure a, ann) CF.:< LL.Global x
      Just t -> pure $ (t, ann) CF.:< LL.Global x
  ann CFT.:< LL.Closure me1 x me2 -> do
    eCtx <- me1
    let tCtx = fst $ extract eCtx
    tArg <- Pure <$> freshType (Right Kind.Type)
    tCtxArg <- pairUp tCtx tArg
    eRet <- pushLocal x tCtxArg me2
    let tRet = fst $ extract eRet
    t <- typeClosure tCtx tArg tRet
    pure $ (t, ann) CF.:< LL.Closure eCtx x eRet
  ann CFT.:< LL.Ap me1 me2 -> do
    ae1 <- me1
    let t1 = fst $ extract ae1
    ae2 <- me2
    let t2 = fst $ extract ae2

    tCtx <- Pure <$> freshType (Right Kind.Type)
    tArg <- Pure <$> freshType (Right Kind.Type)
    tRet <- Pure <$> freshType (Right Kind.Type)

    tCls <- typeClosure tCtx tArg tRet
    pushCons $ TyEq t1 tCls
    pushCons $ TyEq t2 tArg

    pure $ (tRet, ann) CF.:< LL.Ap ae1 ae2
  ann CFT.:< LL.Match me1 mbs -> do
    ae1 <- me1
    let t1 = fst $ extract ae1
    bs <- M.traverseWithKey (goBranch t1) mbs
    let tbs = fst . extract . snd <$> M.elems bs
    tRet <- case tbs of
      [] -> Pure <$> freshType (Right Kind.Type)
      [tRet] -> pure tRet
      tRet : tRest -> do
        -- Unify all the branches
        mapM_ (pushCons . TyEq tRet) tRest
        pure tRet
    pure $ (tRet, ann) CF.:< LL.Match ae1 bs
  where
    goBranch ::
      Ty' g ->
      Maybe g ->
      ([l], m (CF.Cofree (LL.Expr g l) (Ty' g, ann))) ->
      m ([l], CF.Cofree (LL.Expr g l) (Free (Ty.Ty Kind' g) TyVar', ann))
    goBranch t cn (xs, mae) = do
      mc <- join <$> traverse lookupConstructor cn
      (tc, txs) <- case mc of
        Nothing -> pure (t, [])
        Just (tc, txs) -> pure (tc, txs)

      pushCons $ TyEq t tc

      -- We just grab a fresh type when we get too many args
      xtxs <-
        zipWithM
          ( \x mt ->
              (x,)
                <$> maybe (Pure <$> freshType (Right Kind.Type)) pure mt
          )
          xs
          ((Just <$> txs) <> repeat Nothing)

      (xs,)
        <$> foldr
          (uncurry pushLocal)
          mae
          xtxs

lookupConstructor :: (Check g l m) => g -> m (Maybe (Ty' g, [Ty' g]))
lookupConstructor c = asks (M.lookup c . envConstrs)

unknownName :: (Check g l m) => m TyVar'
unknownName = do
  k <- freshKind
  freshType (Left k)

freshKind :: forall g l m. (Check g l m) => m KindVar'
freshKind = do
  k <- state $ \st ->
    ( stFreshKind st,
      st {stFreshKind = succ (stFreshKind st)}
    )
  pushCons $ IsKind k
  pure k

freshType :: (Check g l m) => Kind' -> m TyVar'
freshType k = do
  a <- state $ \st ->
    ( stFreshTy st,
      st {stFreshTy = succ (stFreshTy st)}
    )
  pushCons $ TyKind (Pure a) k
  pure a

pairUp :: (Check g l m) => Ty' g -> Ty' g -> m (Ty' g)
pairUp t1 t2 = do
  gPair <- asks envPair
  pure $
    Free $
      Ty.Ap
        (Free $ Ty.Ap (Free $ Ty.Var gPair) t1)
        t2

typeClosure :: (Check g l m) => Ty' g -> Ty' g -> Ty' g -> m (Ty' g)
typeClosure tCtx tArg tRet = do
  gClosure <- asks envClosure
  pure $
    Free $
      Ty.Ap
        ( Free $
            Ty.Ap
              ( Free $
                  Ty.Ap
                    (Free $ Ty.Var gClosure)
                    tCtx
              )
              tArg
        )
        tRet
