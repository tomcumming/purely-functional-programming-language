module PF.Compile.LambdaLift
  ( Names (..),
    lambdaLift,
  )
where

import Control.Category ((>>>))
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as TCF
import Control.Monad.Writer.Class (MonadWriter, censor, tell)
import Control.Monad.Writer.Strict (runWriter)
import Data.Containers.ListUtils (nubOrd)
import Data.Fix (Fix (Fix))
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Foldable (cata)
import Data.List.NonEmpty qualified as NE
import Data.Sequence qualified as Sq
import Data.Set qualified as S
import PF.Compile.Linearise (Instd, capture)
import PF.Expr.LambdaLifted qualified as LL
import PF.Expr.Linearised qualified as LE

data Names l g = Names
  { namePair :: g,
    nameUnit :: g
  }

type InE l g an = CF.Cofree (LE.ExprF (Instd l) g) an

type OutE l g an = CF.Cofree (LL.ExprF (Instd l) g) an

-- | Lambda lift into closure constructors, preserving the order that free vars
--   appear in the body.
lambdaLift :: (Ord l) => Names l g -> InE l g an -> OutE l g an
lambdaLift ns =
  lambdaLift' ns
    >>> runWriter
    >>> fst

lambdaLift' ::
  forall l g an m.
  (MonadWriter (Sq.Seq (Instd l)) m) =>
  (Ord l) =>
  Names l g ->
  InE l g an ->
  m (OutE l g an)
lambdaLift' ns@Names {namePair} = cata $ \case
  an TCF.:< LE.Local x -> an CF.:< LL.Local x <$ tell (Sq.singleton x)
  an TCF.:< LE.Global x -> an CF.:< LL.Global x & pure
  an TCF.:< LE.Abs pat me -> do
    let (pat', bound) = bindPattern pat & runWriter
    (e, used) <- capture me
    let frees =
          Sq.filter (`S.notMember` bound) used
            & toList
            & nubOrd
            & NE.nonEmpty
    an
      CF.:< LL.Abs
        (tupleTerm ns an frees)
        (LL.Cons namePair [tuplePat ns frees, pat'] & Fix)
        e
      & pure
  an TCF.:< LE.Ap me1 me2 -> LL.Ap <$> me1 <*> me2 & fmap (an CF.:<)
  an TCF.:< LE.Match mec bs ->
    LL.Match <$> mec <*> traverse (uncurry goBranch) bs & fmap (an CF.:<)
  where
    goBranch ::
      LE.Pat (Instd l) g ->
      m (OutE l g an) ->
      m (LL.Pat (Instd l) g, OutE l g an)
    goBranch pat me = do
      let (pat', bound) = bindPattern pat & runWriter
      e <- me & censor (Sq.filter (`S.notMember` bound))
      pure (pat', e)

bindPattern ::
  (MonadWriter (S.Set (Instd l)) m) =>
  LE.Pat (Instd l) g ->
  m (LL.Pat (Instd l) g)
bindPattern = cata $ \case
  LE.Var x -> Fix (LL.Var x) <$ tell (S.singleton x)
  LE.Cons c ps -> sequenceA ps <&> (LL.Cons c >>> Fix)

tupleTerm ::
  Names l g ->
  an ->
  Maybe (NE.NonEmpty (Instd l)) ->
  OutE l g an
tupleTerm Names {namePair, nameUnit} an = \case
  Nothing -> an CF.:< LL.Global nameUnit
  Just xs -> xs <&> (LL.Local >>> (an CF.:<)) & foldr1 pairUp
  where
    pairUp e1 e2 =
      an
        CF.:< LL.Ap
          (an CF.:< LL.Ap (an CF.:< LL.Global namePair) e1)
          e2

tuplePat :: Names l g -> Maybe (NE.NonEmpty (Instd l)) -> LL.Pat (Instd l) g
tuplePat Names {namePair, nameUnit} = \case
  Nothing -> LL.Cons nameUnit [] & Fix
  Just xs -> xs <&> (LL.Var >>> Fix) & foldr1 pairUp
  where
    pairUp p1 p2 = LL.Cons namePair [p1, p2] & Fix
