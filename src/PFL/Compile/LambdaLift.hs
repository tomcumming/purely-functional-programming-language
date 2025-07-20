module PFL.Compile.LambdaLift
  ( lambdaLiftExpr,
  )
where

import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CFT
import Data.Functor.Foldable (cata)
import Data.Map qualified as M
import Data.Set qualified as S
import PFL.Expr.LambdaLifted qualified as LL
import PFL.Expr.Qualified qualified as Q

type QExpr ann = CF.Cofree Q.Expr ann

type LLExpr ann = CF.Cofree LL.Expr ann

lambdaLiftExpr :: (Show ann) => QExpr ann -> LLExpr ann
lambdaLiftExpr = cata $ \case
  (ann CFT.:< Q.Local x) -> ann CF.:< LL.Local x
  (ann CFT.:< Q.Global x) -> ann CF.:< LL.Global x
  (ann CFT.:< Q.Ap e1 e2) -> ann CF.:< LL.Ap e1 e2
  (ann CFT.:< Q.Abs e) ->
    let ys = Q.unAbs $ LL.free e
        ctx = (ann CF.:<) . LL.Local <$> S.toDescList ys
        s =
          M.unions $
            zipWith M.singleton (succ <$> S.toAscList ys) $
              (ann CF.:<) . LL.Local <$> [toEnum 1 :: Q.Di ..]
     in ann CF.:< LL.Closure ctx (LL.subs s e)
