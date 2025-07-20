module PFL.Compile.LambdaLift
  ( lambdaLiftExpr,
  )
where

import Control.Category ((>>>))
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CFT
import Control.Monad.State.Class (MonadState, state)
import Data.Foldable (toList)
import Data.Functor.Foldable (para)
import Data.List (unsnoc)
import Data.Map qualified as M
import Data.Set qualified as S
import PFL.Expr.LambdaLifted qualified as LL
import PFL.Expr.Qualified qualified as Q

type QExpr ann = CF.Cofree Q.Expr ann

type LLExpr ann = CF.Cofree LL.Expr ann

lambdaLiftExpr :: (MonadState LL.GenName m) => QExpr ann -> m (LLExpr ann)
lambdaLiftExpr = para $ \case
  (ann CFT.:< Q.Local x) -> pure $ ann CF.:< LL.Local (LL.Named x)
  (ann CFT.:< Q.Global x) -> pure $ ann CF.:< LL.Global x
  (ann CFT.:< Q.Ap (_, me1) (_, me2)) -> (ann CF.:<) <$> (LL.Ap <$> me1 <*> me2)
  (ann CFT.:< Q.Abs x (eBody, meBody)) -> do
    y <- genName
    let ys = S.delete x $ Q.free eBody
    let closurePattern = makePattern $ LL.Named <$> toList ys
    bound <-
      bindNames
        ann
        (ann CF.:< LL.Local y)
        meBody
        (PPair (LL.Named x) closurePattern)
    pure $ ann CF.:< LL.Closure (closureExpr ann closurePattern) y bound

genName :: (MonadState LL.GenName m) => m LL.Local
genName = state $ \x -> (LL.Anon x, succ x)

data Pattern
  = PUnit
  | PVar LL.Local
  | PPair LL.Local Pattern

makePattern :: [LL.Local] -> Pattern
makePattern =
  unsnoc >>> \case
    Nothing -> PUnit
    Just (xs, y) -> foldr PPair (PVar y) xs

-- | Build up the closure as a tuple
closureExpr :: forall ann. ann -> Pattern -> LLExpr ann
closureExpr ann =
  (ann CF.:<) . \case
    PUnit -> LL.Global "Unit"
    PVar x -> LL.Local x
    PPair x p ->
      LL.Ap
        ( ann
            CF.:< LL.Ap
              (ann CF.:< LL.Global "Pair")
              (ann CF.:< LL.Local x)
        )
        (closureExpr ann p)

-- We pass an action to generate the body so we get a nice order of generated
-- variables...
bindNames ::
  (MonadState LL.GenName m) =>
  ann ->
  LLExpr ann ->
  m (LLExpr ann) ->
  Pattern ->
  m (LLExpr ann)
bindNames ann eArg meBody = \case
  PUnit -> do
    eBody <- meBody
    pure $
      ann
        CF.:< LL.Match
          eArg
          (M.singleton "Unit" ([], eBody))
          Nothing
  -- Perhaps we can check eArg is already named this
  PVar x -> do
    eBody <- meBody
    pure $
      ann
        CF.:< LL.Match -- Not sure we ever get here...
          eArg
          mempty
          (Just (x, eBody))
  PPair x (PVar y) -> do
    eBody <- meBody
    pure $
      ann
        CF.:< LL.Match
          eArg
          (M.singleton "Pair" ([x, y], eBody))
          Nothing
  PPair x p -> do
    y <- genName
    eBody <- bindNames ann (ann CF.:< LL.Local y) meBody p
    pure $
      ann
        CF.:< LL.Match
          eArg
          (M.singleton "Pair" ([x, y], eBody))
          Nothing
