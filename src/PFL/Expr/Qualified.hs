module PFL.Expr.Qualified
  ( Local (..),
    Expr (..),
    anonymise,
    maxAnon,
    subst,
  )
where

import Control.Category ((>>>))
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CFT
import Control.Monad.Reader (MonadReader, asks, local, runReader)
import Data.Foldable (fold)
import Data.Functor.Classes (Show1)
import Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import Data.Functor.Foldable (cata, cataA)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Semigroup (Max (..))
import Data.Set qualified as S
import GHC.Generics (Generic1)
import Optics qualified as O

data Local l
  = Named l
  | Anon Word
  deriving (Eq, Ord, Show)

data Expr g l a
  = Local l
  | Global g
  | Abs l a
  | Ap a a
  | Match a (M.Map (Maybe g) ([l], a))
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic1)
  deriving (Show1) via FunctorClassesDefault (Expr g l)

locals ::
  forall g l l' ann.
  O.Traversal
    (CF.Cofree (Expr g l) ann)
    (CF.Cofree (Expr g l') ann)
    l
    l'
locals = O.traversalVL $ \f -> cata (alg f)
  where
    alg ::
      (Applicative m) =>
      (l -> m l') ->
      CFT.CofreeF (Expr g l) ann (m (CF.Cofree (Expr g l') ann)) ->
      m (CF.Cofree (Expr g l') ann)
    alg f = \case
      ann CFT.:< Local x -> (ann CF.:<) . Local <$> f x
      ann CFT.:< Global x -> pure $ ann CF.:< Global x
      ann CFT.:< Abs x me -> (ann CF.:<) <$> (Abs <$> f x <*> me)
      ann CFT.:< Ap me1 me2 -> (ann CF.:<) <$> (Ap <$> me1 <*> me2)
      ann CFT.:< Match me bs ->
        (ann CF.:<)
          <$> (Match <$> me <*> traverse (uncurry goBranch) bs)
      where
        goBranch xs me = (,) <$> traverse f xs <*> me

anonymise :: CF.Cofree (Expr g l) ann -> CF.Cofree (Expr g (Local l)) ann
anonymise = O.over locals Named

maxAnon :: CF.Cofree (Expr g (Local l)) ann -> Maybe Word
maxAnon = fmap getMax . cata alg
  where
    alg =
      CFT.tailF >>> \case
        Local l -> goLocal l
        Abs x m -> m <> goLocal x
        Match m bs -> m <> foldMap (uncurry goBranch) bs
        e -> fold e

    goBranch xs m = m <> foldMap goLocal xs

    goLocal = \case
      Named {} -> Nothing
      Anon x -> Just $ Max x

subst ::
  forall g l ann.
  (Ord l) =>
  M.Map l (CF.Cofree (Expr g l) ann) ->
  CF.Cofree (Expr g l) ann ->
  CF.Cofree (Expr g l) ann
subst = \s -> flip runReader s . cataA alg
  where
    alg ::
      (MonadReader (M.Map l (CF.Cofree (Expr g l) ann)) m) =>
      CFT.CofreeF (Expr g l) ann (m (CF.Cofree (Expr g l) ann)) ->
      m (CF.Cofree (Expr g l) ann)
    alg = \case
      ann CFT.:< Local x -> asks (fromMaybe (ann CF.:< Local x) . (M.!? x))
      ann CFT.:< Abs x me -> (ann CF.:<) . Abs x <$> local (M.delete x) me
      ann CFT.:< Match me bs ->
        (ann CF.:<)
          <$> (Match <$> me <*> traverse (uncurry goBranch) bs)
      ann CFT.:< e -> (ann CF.:<) <$> sequence e
    goBranch xs me = (xs,) <$> local (flip M.withoutKeys (S.fromList xs)) me
