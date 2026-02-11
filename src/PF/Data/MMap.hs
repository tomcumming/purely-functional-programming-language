module PF.Data.MMap
  ( MMap,
    minView,
    minViewKey,
    cons,
    snoc,
    intersect,
    mapKeysInjective,
  )
where

import Control.Category ((>>>))
import Control.Monad ((>=>))
import Data.Function ((&))
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Seq
import Optics qualified as O

newtype MMap k v = MMap {getMap :: M.Map k (Seq.Seq v)}
  deriving newtype (Show)

deriving instance Functor (MMap k)

deriving instance Foldable (MMap k)

deriving instance Traversable (MMap k)

seqNonEmpty :: Seq.Seq a -> Maybe (Seq.Seq a)
seqNonEmpty s
  | Seq.null s = Nothing
  | otherwise = Just s

minView :: (Ord k) => O.Prism' (MMap k v) ((k, v), MMap k v)
minView =
  O.prism'
    ( uncurry
        ( \(k, v) ->
            getMap
              >>> M.insertWith (<>) k (Seq.singleton v)
              >>> MMap
        )
    )
    ( (getMap >>> M.minViewWithKey) >=> \case
        ((k, v Seq.:<| vs), rest) ->
          Just
            ( (k, v),
              seqNonEmpty vs
                & maybe id (M.insert k)
                & (rest &)
                & MMap
            )
        _ -> error "Internal error: empty Seq"
    )

minViewKey :: (Ord k) => k -> O.Prism' (MMap k v) (v, MMap k v)
minViewKey k =
  O.prism'
    ( uncurry
        ( \v ->
            getMap
              >>> M.insertWith (<>) k (Seq.singleton v)
              >>> MMap
        )
    )
    ( \(MMap mm) ->
        mm M.!? k >>= \case
          (v Seq.:<| Seq.Empty) -> Just (v, M.delete k mm & MMap)
          (v Seq.:<| vs) -> Just (v, M.insert k vs mm & MMap)
          _ -> error "Internal error: empty Seq"
    )

cons :: (Ord k) => k -> v -> MMap k v -> MMap k v
cons k v mm = O.review minView ((k, v), mm)

snoc :: (Ord k) => MMap k v -> k -> v -> MMap k v
snoc (MMap mm) k v =
  M.insertWith (flip (<>)) k (Seq.singleton v) mm
    & MMap

instance (Ord k) => Semigroup (MMap k v) where
  m1 <> m2 = case O.preview minView m2 of
    Nothing -> m1
    Just ((k, v), m2') -> snoc m1 k v <> m2'

instance (Ord k) => Monoid (MMap k v) where
  mempty = MMap mempty

intersect ::
  (Ord k) =>
  MMap k v ->
  MMap k v' ->
  ( MMap k v,
    MMap k v',
    MMap k (v, v')
  )
intersect m1 m2 = case O.preview minView m1 of
  Just ((k, v1), m1') -> case O.preview (minViewKey k) m2 of
    Nothing ->
      let (es1, es2, is) = intersect m1' m2
       in (cons k v1 es1, es2, is)
    Just (v2, m2') ->
      let (es1, es2, is) = intersect m1' m2'
       in (es1, es2, cons k (v1, v2) is)
  Nothing -> (mempty, m2, mempty)

mapKeysInjective :: (Ord k') => (k -> k') -> MMap k v -> MMap k' v
mapKeysInjective f = getMap >>> M.mapKeys f >>> MMap
