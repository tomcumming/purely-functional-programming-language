module PFL.Compile.Qualify (qualify) where

import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CFT
import Control.Monad.RWS (MonadRWS, evalRWS)
import Control.Monad.RWS.Class (asks, local)
import Data.Functor.Foldable (cataA)
import Data.Set qualified as S
import Data.Text qualified as T
import PFL.Expr.In qualified as In
import PFL.Expr.Qualified qualified as Q

type InExpr ann = CF.Cofree In.Expr ann

type QExpr ann = CF.Cofree (Q.Expr T.Text T.Text) ann

qualify :: forall ann. InExpr ann -> QExpr ann
qualify = \inExpr ->
  fst $
    evalRWS (cataA alg inExpr) (mempty :: S.Set T.Text) 0
  where
    alg ::
      (MonadRWS (S.Set T.Text) () Int m) =>
      CFT.CofreeF In.Expr ann (m (QExpr ann)) ->
      m (QExpr ann)
    alg = \case
      ann CFT.:< In.EVar x ->
        asks (S.member x) >>= \case
          True -> pure $ ann CF.:< Q.Local x
          False -> pure $ ann CF.:< Q.Global x
      ann CFT.:< In.Abs x me ->
        (ann CF.:<) . Q.Abs x
          <$> local (S.insert x) me
      ann CFT.:< In.Ap me1 me2 -> (ann CF.:<) <$> (Q.Ap <$> me1 <*> me2)
      ann CFT.:< In.Match me bs ->
        (ann CF.:<)
          <$> (Q.Match <$> me <*> traverse (uncurry goBranch) bs)

    -- TODO handle duplicate names in pattern
    goBranch xs me = (xs,) <$> local (<> S.fromList xs) me
