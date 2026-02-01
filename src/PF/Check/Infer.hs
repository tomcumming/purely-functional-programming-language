module PF.Check.Infer
  ( Typed,
    InE,
    OutE,
  )
where

import Control.Comonad.Cofree qualified as CF
import PF.Expr.LambdaLifted qualified as LL
import PF.Unify.Data.Env (Ty)

type Typed g an = (Ty g, an)

type InE l g an = CF.Cofree (LL.ExprF l g) (Typed g an)

type OutE l g an = InE l g (Typed g an)
