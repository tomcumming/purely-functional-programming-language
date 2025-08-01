module PFL.Compile.LambdaLift (lambdaLift, Names (..)) where

import Control.Category ((>>>))
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CFT
import Data.Bifunctor (second)
import Data.Functor.Foldable (cata)
import Data.Map qualified as M
import Data.Set qualified as S
import PFL.Expr.LambdaLifted qualified as L
import PFL.Expr.Qualified qualified as Q

data Names g = Names
  { nmUnit :: g,
    nmPair :: g
  }

type QExpr g l ann = CF.Cofree (Q.Expr g (Q.Local l)) ann

type LExpr g l ann = CF.Cofree (L.Expr g (Q.Local l)) ann

lambdaLift ::
  forall g l ann.
  (Ord l) =>
  Names g ->
  QExpr g l ann ->
  LExpr g l ann
lambdaLift Names {nmPair, nmUnit} inExpr = snd $ cata algMain inExpr
  where
    algMain ::
      CFT.CofreeF
        (Q.Expr g (Q.Local l))
        ann
        (S.Set (Q.Local l), LExpr g l ann) ->
      (S.Set (Q.Local l), LExpr g l ann)
    algMain = \case
      ann CFT.:< Q.Local l -> (S.singleton l, ann CF.:< L.Local l)
      ann CFT.:< Q.Global g -> (mempty, ann CF.:< L.Global g)
      ann CFT.:< Q.Abs x (ls, e) ->
        let ls' = S.delete x ls
         in ( S.delete x ls',
              ann CF.:< L.Closure (pairUp ann ls') x (unPair ann x e ls')
            )
      ann CFT.:< Q.Ap (ls1, e1) (ls2, e2) -> (ls1 <> ls2, ann CF.:< L.Ap e1 e2)
      ann CFT.:< Q.Match (ls1, e1) bs ->
        let ls2 = foldMap (fst . snd) bs
            bs' = second snd <$> bs
         in (ls1 <> ls2, ann CF.:< L.Match e1 bs')

    fresh = Q.Anon $ maybe 0 succ $ Q.maxAnon inExpr

    pairUp :: ann -> S.Set (Q.Local l) -> LExpr g l ann
    pairUp ann =
      S.maxView >>> \case
        Nothing -> ann CF.:< L.Global nmUnit
        Just (lMax, ls) ->
          foldr
            (\l -> callPair ann (ann CF.:< L.Local l))
            (ann CF.:< L.Local lMax)
            ls

    unPair ::
      ann ->
      Q.Local l ->
      LExpr g l ann ->
      S.Set (Q.Local l) ->
      LExpr g l ann
    unPair ann lArg eBody =
      S.maxView
        >>> \case
          Nothing ->
            unPairArg lArg fresh lArg $
              ann
                CF.:< L.Match
                  (ann CF.:< L.Local fresh)
                  (M.singleton (Just nmUnit) ([], eBody))
          Just (lMax, ls) -> case S.maxView ls of
            Nothing -> unPairArg lArg lMax lArg eBody
            Just (lMax2, ls2) ->
              unPairArg lArg fresh lArg $
                foldr
                  (\l -> unPairArg fresh l fresh)
                  (unPairArg fresh lMax2 lMax eBody)
                  ls2
      where
        unPairArg l1 l2 l3 e =
          ann
            CF.:< L.Match
              (ann CF.:< L.Local l1)
              (M.singleton (Just nmPair) ([l2, l3], e))

    callPair ann e1 e2 = ann CF.:< L.Ap (callFn ann nmPair e1) e2
    callFn ann f e = ann CF.:< L.Ap (ann CF.:< L.Global f) e
