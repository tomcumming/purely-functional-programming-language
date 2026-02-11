module PF.Data.Row
  ( Row,
    kvs,
    rtail,
    empty,
    make,
    push,
    pop,
    popMin,
    zipRows,
  )
where

import Data.Function ((&))
import Optics qualified as O
import PF.Data.MMap (MMap)
import PF.Data.MMap qualified as MM

data Row k v t = Row
  { rowKvs :: MMap k v,
    rowTail :: t
  }
  deriving (Show)

kvs :: O.Lens (Row k v t) (Row k' v' t) (MMap k v) (MMap k' v')
kvs = O.lens rowKvs (\row rowKvs -> row {rowKvs})

rtail :: O.Lens (Row k v a) (Row k v t) a t
rtail = O.lens rowTail (\row rowTail -> row {rowTail})

empty :: (Ord k) => t -> Row k v t
empty rowTail = Row {rowKvs = mempty, rowTail}

make :: MMap k v -> t -> Row k v t
make = Row

push :: (Ord k) => k -> v -> Row k v t -> Row k v t
push k v = O.over kvs (MM.cons k v)

pop :: (Ord k) => k -> Row k v t -> Either t (v, Row k v t)
pop k row@Row {rowKvs} =
  MM.unconsKey k rowKvs & \case
    Nothing -> rowTail row & Left
    Just (v, rowKvs') -> Right (v, O.set kvs rowKvs' row)

popMin :: (Ord k) => Row k v t -> Either t ((k, v), Row k v t)
popMin row@Row {rowKvs, rowTail} =
  MM.uncons rowKvs & \case
    Nothing -> Left rowTail
    Just (kv, rowKvs') -> Right (kv, O.set kvs rowKvs' row)

zipRows ::
  (Ord k) =>
  Row k v t ->
  Row k v' t' ->
  ( MMap k v,
    MMap k v',
    Row k (v, v') (t, t')
  )
zipRows (Row kvs1 t1) (Row kvs2 t2) =
  let (es1, es2, is) = MM.intersect kvs1 kvs2
   in (es1, es2, make is (t1, t2))
