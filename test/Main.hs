module Main (main) where

import Control.Category ((>>>))
import Control.Comonad.Cofree qualified as CF
import Control.Comonad.Trans.Cofree qualified as CFF
import Control.Monad (forM_)
import Data.Fix (Fix (Fix))
import Data.Function ((&))
import Data.Functor.Foldable (cata)
import Data.Text qualified as T
import PF.Compile.Linearise qualified as Linearise
import Test.Sexp qualified as Sexp
import Test.Sexp.Expr ()

names :: Linearise.Names T.Text
names =
  Linearise.Names
    { nameUnit = "Unit",
      nameCopy = "copy",
      nameTuple = "Tuple",
      nameDrop = "drop"
    }

test1 :: Sexp.Sexp
test1 = ["lambda", ["i0", "i0"]]

test2 :: Sexp.Sexp
test2 = ["lambda", "hi"]

test3 :: Sexp.Sexp
test3 = ["lambda", ["lambda", ["i1", "i1"]]]

cf2fix :: (Functor f) => CF.Cofree f a -> Fix f
cf2fix = cata (CFF.tailF >>> Fix)

fix2cf :: (Functor f) => Fix f -> CF.Cofree f ()
fix2cf = cata (() CF.:<)

main :: IO ()
main = do
  forM_ ([test1, test2, test3] :: [Sexp.Sexp]) $ \test -> do
    print test
    e <-
      Sexp.from test
        & either (T.unpack >>> fail) (fix2cf >>> pure)
    let e' = Linearise.getLinearExpr $ Linearise.linearise names e
    print $ Sexp.into $ cf2fix e'
    putStrLn ""
