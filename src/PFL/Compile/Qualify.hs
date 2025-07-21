module PFL.Compile.Qualify
  ( qualify,
  )
where

import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CFT
import Control.Monad.Reader (asks, local, runReader)
import Data.Functor.Foldable (cataA)
import Data.Set qualified as S
import Data.Text qualified as T
import PFL.Expr.In qualified as In
import PFL.Expr.Qualified qualified as Q

type InExpr ann = CF.Cofree In.Expr ann

type QExpr ann = CF.Cofree Q.Expr ann

qualify :: InExpr ann -> QExpr ann
qualify = flip runReader (mempty :: S.Set T.Text) . cataA go
  where
    go = \case
      ann CFT.:< In.EVar x ->
        asks (S.member x) >>= \case
          True -> pure $ ann CF.:< Q.Local x
          False -> pure $ ann CF.:< Q.Global x
      ann CFT.:< In.Abs x me -> (ann CF.:<) . Q.Abs x <$> local (S.insert x) me
      ann CFT.:< In.Ap me1 me2 -> (ann CF.:<) <$> (Q.Ap <$> me1 <*> me2)
