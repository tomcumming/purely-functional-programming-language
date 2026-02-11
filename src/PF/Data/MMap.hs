module PF.Data.MMap
  ( MMap,
    uncons,
    unconsKey,
    cons,
    snoc,
    intersect,
    mapKeysInjective,
  )
where

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Seq

newtype MMap k v = MMap {getMap :: M.Map k (Seq.Seq v)}
  deriving newtype (Show)

deriving instance Functor (MMap k)

deriving instance Foldable (MMap k)

deriving instance Traversable (MMap k)

seqNonEmpty :: Seq.Seq a -> Maybe (Seq.Seq a)
seqNonEmpty s
  | Seq.null s = Nothing
  | otherwise = Just s

uncons :: (Ord k) => MMap k v -> Maybe ((k, v), MMap k v)
uncons (MMap mm) =
  M.minViewWithKey mm >>= \case
    ((k, v Seq.:<| vs), rest) -> do
      Just
        ( (k, v),
          seqNonEmpty vs
            & maybe id (M.insert k)
            & (rest &)
            & MMap
        )
    _ -> error "Internal error: empty Seq"

unconsKey :: (Ord k) => k -> MMap k v -> Maybe (v, MMap k v)
unconsKey k (MMap mm) =
  mm M.!? k >>= \case
    (v Seq.:<| vs) ->
      Just
        ( v,
          seqNonEmpty vs
            & maybe (M.delete k) (M.insert k)
            & (mm &)
            & MMap
        )
    _ -> error "Internal error: empty Seq"

cons :: (Ord k) => k -> v -> MMap k v -> MMap k v
cons k v = getMap >>> M.insertWith (<>) k (Seq.singleton v) >>> MMap

snoc :: (Ord k) => MMap k v -> k -> v -> MMap k v
snoc (MMap mm) k v =
  M.insertWith (flip (<>)) k (Seq.singleton v) mm
    & MMap

instance (Ord k) => Semigroup (MMap k v) where
  m1 <> m2 = case uncons m2 of
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
intersect m1 m2 = case uncons m1 of
  Just ((k, v1), m1') -> case unconsKey k m2 of
    Nothing ->
      let (es1, es2, is) = intersect m1' m2
       in (cons k v1 es1, es2, is)
    Just (v2, m2') ->
      let (es1, es2, is) = intersect m1' m2'
       in (es1, es2, cons k (v1, v2) is)
  Nothing -> (mempty, m2, mempty)

mapKeysInjective :: (Ord k') => (k -> k') -> MMap k v -> MMap k' v
mapKeysInjective f = getMap >>> M.mapKeys f >>> MMap
