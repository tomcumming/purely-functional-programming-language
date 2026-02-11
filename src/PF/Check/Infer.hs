module PF.Check.Infer (infer) where

import Control.Comonad.Cofree qualified as CF
import Control.Monad.State.Class (MonadState)
import PF.Expr.Core qualified as Core
import PF.Expr.In qualified as In
import PF.Unify.Subst (Subst)

type InE x an = CF.Cofree (In.ExprF x) an

type OutE x an = CF.Cofree (Core.ExprF x) an

infer ::
  (MonadState (Subst x) m) =>
  InE x an -> m (OutE x an)
infer = undefined
