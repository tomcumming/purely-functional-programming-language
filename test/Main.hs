module Main (main) where

import Test.PF.Linearise qualified as Linearise
import Test.Sexp.Expr ()
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "PF"
      [Linearise.tests]
