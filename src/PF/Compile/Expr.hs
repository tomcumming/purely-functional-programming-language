module PF.Compile.Expr
  ( InE,
    OutE,
    Names (..),
    translate,
  )
where

import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as TCF
import Data.Fix (Fix (..))
import Data.Functor.Foldable (cata)
import PF.Expr.Core qualified as Out
import PF.Expr.In qualified as In

type InE c an = CF.Cofree (In.ExprF c) an

type OutE c an = CF.Cofree (Out.ExprF c) an

data Names c = Names
  {nameLambda :: c}
  deriving (Show)

translate :: Names c -> InE c an -> OutE c an
translate Names {nameLambda} = cata $ \case
  an TCF.:< In.Var x -> an CF.:< Out.Global x
  an TCF.:< In.Comp e1 e2 ->
    an
      CF.:< Out.Lambda
        (Fix Out.Var)
        ( an
            CF.:< Out.Ap
              e2
              (an CF.:< Out.Ap e1 (an CF.:< Out.Local (toEnum 0)))
        )
  an TCF.:< In.Lambda e ->
    an
      CF.:< Out.Ap
        (an CF.:< Out.Global nameLambda)
        e
