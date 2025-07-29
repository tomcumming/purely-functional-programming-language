module PFL.Compile.LambdaLift
  ( lambdaLift,
    Names (..),
  )
where

import Control.Category ((>>>))
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CFT
import Data.Functor.Foldable (cata)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import PFL.Compile.Linearise (Linearised, unLinearised)
import PFL.Expr.LambdaLifted qualified as L
import PFL.Expr.Qualified qualified as Q

type LExpr g ann = CF.Cofree (L.Expr g T.Text) ann

data Names g l = Names
  { nmUnit :: !g,
    nmPair :: !g,
    nmClosure :: !l
  }

lambdaLift ::
  forall g ann.
  Names g T.Text ->
  Linearised g ann ->
  LExpr g ann
lambdaLift Names {..} = cata alg . Q.annotateFree . unLinearised
  where
    alg ::
      CFT.CofreeF (Q.Expr g T.Text) (S.Set T.Text, ann) (LExpr g ann) ->
      LExpr g ann
    alg ((fs, ann) CFT.:< inExpr) = case inExpr of
      Q.Local x -> ann CF.:< L.Local x
      Q.Global x -> ann CF.:< L.Global x
      Q.Abs x e ->
        let fs' = S.delete x fs
         in ann CF.:< L.Closure (makeClosure ann fs') x (bindClosure ann x fs' e)
      Q.Ap e1 e2 -> ann CF.:< L.Ap e1 e2
      Q.Match e bs -> ann CF.:< L.Match e bs

    makeClosure :: ann -> S.Set T.Text -> LExpr g ann
    makeClosure ann =
      S.maxView >>> \case
        Nothing -> ann CF.:< L.Global nmUnit
        Just (x, xs) ->
          foldr
            ( \y e ->
                ann
                  CF.:< L.Ap
                    (ann CF.:< L.Ap (ann CF.:< L.Global nmPair) (ann CF.:< L.Local y))
                    e
            )
            (ann CF.:< L.Local x)
            xs

    bindClosure ::
      ann ->
      T.Text ->
      S.Set T.Text ->
      LExpr g ann ->
      LExpr g ann
    bindClosure ann x fs e =
      ann
        CF.:< L.Match
          (ann CF.:< L.Local x)
          (M.singleton (Just nmPair) ([x, ctx], bindNames ann ctx e fs))
      where
        ctx = freshNameLike fs nmClosure

    bindNames :: ann -> T.Text -> LExpr g ann -> S.Set T.Text -> LExpr g ann
    bindNames ann ctx e = go . S.toList
      where
        go :: [T.Text] -> LExpr g ann
        go =
          (ann CF.:<) . L.Match (ann CF.:< L.Local ctx) . \case
            [] -> M.singleton (Just nmUnit) ([], e)
            [x] -> M.singleton Nothing ([x], e)
            x : xs -> M.singleton (Just nmPair) ([x, ctx], go xs)

freshNameLike :: S.Set T.Text -> T.Text -> T.Text
freshNameLike fs x
  | S.notMember x fs = x
  | otherwise = go 0
  where
    go (n :: Int)
      | name <- x <> T.pack (show n),
        S.notMember name fs =
          name
      | otherwise = go (succ n)
