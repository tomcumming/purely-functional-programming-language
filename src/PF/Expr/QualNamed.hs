module PF.Expr.QualNamed (Expr (..), subst) where

import Control.Category ((>>>))
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CFF
import Control.Monad (forM)
import Control.Monad.Reader (asks, local, runReader)
import Data.Functor.Classes (Show1)
import Data.Functor.Classes.Generic (FunctorClassesDefault (..))
import Data.Functor.Foldable (cata)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import GHC.Generics (Generic1)

data Expr l g a
  = Local l
  | Global g
  | Abs l a
  | Ap a a
  | Match a (M.Map (Maybe g) ([l], a))
  deriving stock (Show, Functor, Generic1)
  deriving (Show1) via FunctorClassesDefault (Expr l g)

subst ::
  (Ord l) =>
  M.Map l (CF.Cofree (Expr l g) ann) ->
  CF.Cofree (Expr l g) ann ->
  CF.Cofree (Expr l g) ann
subst s = cata go >>> flip runReader s
  where
    go (ann CFF.:< inExpr) = case inExpr of
      Local l -> asks $ (M.!? l) >>> fromMaybe (ann CF.:< Local l)
      Global g -> pure $ ann CF.:< Global g
      Abs l me -> (ann CF.:<) . Abs l <$> local (M.delete l) me
      Ap me1 me2 -> (ann CF.:<) <$> (Ap <$> me1 <*> me2)
      Match me1 mbs -> do
        e1 <- me1
        bs <- forM mbs $ \(ls, me) ->
          (ls,)
            <$> local (`M.withoutKeys` S.fromList ls) me
        pure $ ann CF.:< Match e1 bs
