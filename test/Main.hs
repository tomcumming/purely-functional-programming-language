module Main (main) where

import Control.Comonad.Cofree qualified as CF
import Data.Text qualified as T
import PF.Compile.Linearise qualified as Linearise
import PF.Expr.Qualified as Q

names :: Linearise.Names T.Text
names =
  Linearise.Names
    { nameUnit = "Unit",
      nameCopy = "copy",
      nameTuple = "Tuple",
      nameDrop = "drop"
    }

test1 :: CF.Cofree (Q.Expr T.Text) ()
test1 =
  ()
    CF.:< Q.Abs
      ( ()
          CF.:< Q.Ap
            (() CF.:< Q.Local (toEnum 0))
            (() CF.:< Q.Local (toEnum 0))
      )

test2 :: CF.Cofree (Expr T.Text) ()
test2 = () CF.:< Q.Abs (() CF.:< Q.Global "hi")

test3 :: CF.Cofree (Q.Expr T.Text) ()
test3 =
  ()
    CF.:< Q.Abs
      ( ()
          CF.:< Q.Abs
            ( ()
                CF.:< Q.Ap
                  (() CF.:< Q.Local (toEnum 1))
                  (() CF.:< Q.Local (toEnum 1))
            )
      )

main :: IO ()
main = do
  print test3
  let test3' = Linearise.getLinearExpr $ Linearise.linearise names test3
  print test3'

{-

() :< Abs
  (() :< Match
    (() :< Ap (() :< Global "drop") (() :< Local i0))
    (("Unit",0),() :< Global "hi")
    Nothing)

() :< Abs
  (() :< Match
    (() :< Ap
      (() :< Global "copy")
      (() :< Local i0))
    (("Tuple",2),() :< Ap (() :< Local i0) (() :< Local i1))
    Nothing)
-}
