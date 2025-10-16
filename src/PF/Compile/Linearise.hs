module PF.Compile.Linearise
  ( Names (..),
    LinearExpr,
    getLinearExpr,
    linearise,
  )
where

import Control.Category ((>>>))
import Control.Comonad (extract)
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CFF
import Control.Monad (forM)
import Control.Monad.Reader (runReader)
import Control.Monad.Reader.Class (MonadReader, local)
import Data.Function ((&))
import Data.Functor.Foldable (para)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Sq
import Data.Set qualified as S
import PF.Compile.LambdaLift (Ann, QExpr, annotateFree, growSubst, lookupVar, shrinkFrees)
import PF.Expr.Qualified qualified as Q

data Names g = Names
  { nameUnit :: g,
    nameTuple :: g,
    nameDrop :: g,
    nameCopy :: g
  }

newtype LinearExpr g ann = LinearExpr
  {getLinearExpr :: QExpr g ann}

newtype Unique = Unique Word
  deriving newtype (Eq, Ord, Enum)

linearise :: Names g -> QExpr g ann -> LinearExpr g ann
linearise names =
  annotateFree
    >>> linearise' names
    >>> flip runReader mempty
    >>> LinearExpr

linearise' ::
  forall g ann m.
  (MonadReader (Sq.Seq Q.Idx) m) =>
  Names g ->
  QExpr g (Ann ann) ->
  m (QExpr g ann)
linearise' names = para $ \((_, ann) CFF.:< inExpr) -> case inExpr of
  Q.Local l -> do
    l' <- lookupVar l
    pure $ ann CF.:< Q.Local l'
  Q.Global g -> pure (ann CF.:< Q.Global g)
  Q.Abs (ine, me) -> do
    let us = extractFrees ine
    e <- local (growSubst 1) me
    pure $ ann CF.:< Q.Abs (doDrop names ann us (toEnum 0) e)
  Q.Ap (ine1, me1) (ine2, me2) -> do
    let is = extractFrees ine1 `S.intersection` extractFrees ine2
    e1 <- local (substCopy is 0) me1
    e2 <- local (substCopy is 1) me2
    pure $ doCopies names ann is $ ann CF.:< Q.Ap e1 e2
  Q.Match (ine1, me1) mbs -> do
    let mus = flip foldMap mbs $
          \(n, (ine, _)) -> shrinkFrees n $ extractFrees ine
    let is = extractFrees ine1 `S.intersection` mus
    e1 <- local (substCopy is 0) me1
    bs <- forM mbs $ \(n, (ine, me)) -> do
      let us = extractFrees ine
      e <- local (substCopy is 1 >>> growSubst (fromIntegral n)) me
      pure (n, doDrops names ann us n e)
    pure $ doCopies names ann is $ ann CF.:< Q.Match e1 bs

extractFrees :: QExpr g (Ann ann) -> S.Set Q.Idx
extractFrees = extract >>> fst

substCopy :: S.Set Q.Idx -> Int -> Sq.Seq Q.Idx -> Sq.Seq Q.Idx
substCopy is off =
  let d = S.size is * 2 & fromIntegral
      s =
        zip (fromEnum <$> S.toAscList is) [toEnum $ o * 2 + off | o <- [0 ..]]
          & M.fromList
   in Sq.mapWithIndex (\i i' -> s M.!? i & fromMaybe (Q.growIdx d i'))

doDrop :: Names g -> ann -> S.Set Q.Idx -> Q.Idx -> QExpr g ann -> QExpr g ann
doDrop Names {nameDrop, nameUnit} ann us l e
  | S.member l us = e
  | otherwise =
      ann
        CF.:< Q.Match
          (mkCall ann nameDrop (ann CF.:< Q.Local l))
          (M.singleton (Just nameUnit) (0, e))

doDrops ::
  Names g ->
  ann ->
  S.Set Q.Idx ->
  Word ->
  QExpr g ann ->
  CF.Cofree (Q.Expr g) ann
doDrops names ann us n e =
  foldr
    (doDrop names ann us . toEnum . fromIntegral)
    e
    [0 .. pred n]

doCopies :: Names g -> ann -> S.Set Q.Idx -> QExpr g ann -> QExpr g ann
doCopies Names {nameCopy, nameTuple} ann is eOut =
  foldr
    ( \i e ->
        ann
          CF.:< Q.Match
            (mkCall ann nameCopy (ann CF.:< Q.Local i))
            (M.singleton (Just nameTuple) (2, e))
    )
    eOut
    (S.toDescList is)

mkCall :: ann -> g -> QExpr g ann -> QExpr g ann
mkCall ann g e = ann CF.:< Q.Ap (ann CF.:< Q.Global g) e
