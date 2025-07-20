module PF.Compile.Linearise
  ( Inst,
    Instd,
    Names (..),
    linearise,
    capture,
  )
where

import Control.Category ((>>>))
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as TCF
import Control.Monad (forM)
import Control.Monad.Reader (runReader)
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Control.Monad.State.Class (state)
import Control.Monad.State.Strict (evalStateT)
import Control.Monad.Writer.Class (MonadWriter, censor, listen, tell)
import Control.Monad.Writer.Strict (runWriterT)
import Data.Bifunctor (second)
import Data.Fix (Fix (..))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Foldable (cata)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import PF.Expr.In qualified as In
import PF.Expr.Linearised qualified as LE
import Prelude hiding (unzip)

newtype Inst = Inst Int deriving newtype (Eq, Ord, Enum, Show)

type Instd l = (l, Inst)

data Names l g = Names
  { nameUnit :: g,
    namePair :: g,
    nameCopy :: g,
    nameDrop :: g,
    nameBlank :: l
  }

type InE l g an = CF.Cofree (In.ExprF l) an

type OutE l an = CF.Cofree (LE.ExprF (Instd l) l) an

linearise :: (Ord l) => Names l l -> InE l l an -> OutE l an
linearise ns =
  linearise' ns
    >>> runWriterT
    >>> fmap fst
    >>> flip runReader mempty

linearise' ::
  forall l an m.
  (MonadReader (M.Map l Inst) m) =>
  (MonadWriter (M.Map l Inst) m) =>
  (Ord l) =>
  Names l l -> InE l l an -> m (OutE l an)
linearise' ns = cata $ \case
  an TCF.:< In.Var x ->
    asks (M.!? x) >>= \case
      Nothing -> an CF.:< LE.Global x & pure
      Just i -> an CF.:< LE.Local (x, i) <$ tell (M.singleton x i)
  an TCF.:< In.Abs pat me -> do
    (unused, pat', e) <- bindPattern pat me
    an CF.:< LE.Abs pat' (dropUnused ns an unused e) & pure
  an TCF.:< In.Ap me1 me2 -> do
    (e1, used1) <- capture me1
    let rebound = used1 <&> succ
    (e2, used2) <- local (rebound <>) me2 & capture
    used1 <> used2 & tell -- Keep lhs names
    let copies = M.intersectionWith (,) used1 used2
    an CF.:< LE.Ap e1 e2
      & doCopies ns an copies
      & pure
  an TCF.:< In.Match mec mbs -> do
    (ec, used1) <- capture mec
    let rebound = used1 <&> succ
    (bs, used2) <- local (rebound <>) (goBranches an mbs) & capture
    used1 <> used2 & tell -- Keep from condition
    let copies = M.intersectionWith (,) used1 used2
    an CF.:< LE.Match ec bs
      & doCopies ns an copies
      & pure
  where
    goBranches ::
      an ->
      [(In.Pat l, m (OutE l an))] ->
      m [(LE.Pat (Instd l) l, OutE l an)]
    goBranches an mbs = do
      bs <- forM mbs $ \(pat, e) -> goBranch an pat e & capture
      let allUsed = foldMap snd bs
      tell allUsed
      fmap (uncurry (dropBranchUnused an allUsed)) bs & pure

    goBranch ::
      an ->
      In.Pat l ->
      m (OutE l an) ->
      m (LE.Pat (Instd l) l, OutE l an)
    goBranch an pat me = do
      (unused, pat', e) <- bindPattern pat me
      pure (pat', dropUnused ns an unused e)

    dropBranchUnused ::
      an ->
      M.Map l Inst ->
      (LE.Pat (Instd l) l, OutE l an) ->
      M.Map l Inst ->
      (LE.Pat (Instd l) l, OutE l an)
    dropBranchUnused an allUsed (pat, e) branchUsed =
      (pat, dropUnused ns an (allUsed M.\\ branchUsed) e)

    bindPattern pat me = do
      (pat', bound) <- runWriterT (bindPat ns pat)
      (e, used) <- local (bound <>) me & capture
      let unused = bound M.\\ used
      let frees = used M.\\ bound
      tell frees
      pure (unused, pat', e)

capture :: (MonadWriter xs m) => m a -> m (a, xs)
capture = listen >>> censor (const mempty)

bindPat ::
  (MonadReader (M.Map l Inst) m) =>
  (MonadWriter (M.Map l Inst) m) =>
  (Ord l) =>
  Names l l ->
  In.Pat l ->
  m (LE.Pat (Instd l) l)
bindPat Names {nameBlank} = fmap (`evalStateT` toEnum 0) $ cata $ \case
  In.Blank -> do
    i <- state $ \i -> (i, succ i)
    M.singleton nameBlank i & tell
    LE.Var (nameBlank, i) & Fix & pure
  In.Named x -> do
    i <-
      asks (M.!? x) <&> \case
        Nothing -> toEnum 0
        Just i -> i
    M.singleton x i & tell
    LE.Var (x, i) & Fix & pure
  In.Cons c ps -> sequenceA ps <&> (LE.Cons c >>> Fix)

dropUnused :: Names l l -> an -> M.Map l Inst -> OutE l an -> OutE l an
dropUnused ns@Names {nameDrop, nameUnit} an =
  M.assocs >>> NE.nonEmpty >>> \case
    Nothing -> id
    Just xs -> \e ->
      an
        CF.:< LE.Match
          (an CF.:< LE.Ap (an CF.:< LE.Global nameDrop) (termTuple ns an xs))
          [(Fix (LE.Cons nameUnit []), e)]

doCopies :: Names l l -> an -> M.Map l (Inst, Inst) -> OutE l an -> OutE l an
doCopies ns@Names {nameCopy} an =
  M.assocs >>> NE.nonEmpty >>> \case
    Nothing -> id
    Just xys -> \e ->
      an
        CF.:< LE.Match
          (an CF.:< LE.Ap (an CF.:< LE.Global nameCopy) (copyTerm xys))
          [(copyPat xys, e)]
  where
    copyTerm = fmap (second fst) >>> termTuple ns an
    copyPat = fmap (second snd) >>> patTuple ns

termTuple :: forall l an. Names l l -> an -> NE.NonEmpty (l, Inst) -> OutE l an
termTuple Names {namePair} an =
  fmap (LE.Local >>> (an CF.:<))
    >>> foldr1 pairUp
  where
    pairUp :: OutE l an -> OutE l an -> OutE l an
    pairUp e1 e2 =
      an
        CF.:< LE.Ap
          (an CF.:< LE.Ap (an CF.:< LE.Global namePair) e1)
          e2

patTuple :: forall l. Names l l -> NE.NonEmpty (l, Inst) -> LE.Pat (l, Inst) l
patTuple Names {namePair} =
  fmap (LE.Var >>> Fix)
    >>> foldr1 pairUp
  where
    pairUp :: LE.Pat (l, Inst) l -> LE.Pat (l, Inst) l -> LE.Pat (l, Inst) l
    pairUp e1 e2 =
      LE.Cons namePair [e1, e2] & Fix
