module Main (main) where

import Control.Comonad.Cofree qualified as CF
import Control.Monad.Reader (runReader)
import Data.Text qualified as T
import PFL.Compile.LambdaLift qualified as LambdaLift
import PFL.Compile.Linearise qualified as Linearise
import PFL.Compile.Qualify (qualify)
import PFL.Compile.Thread qualified as Thread
import PFL.Expr.In qualified as In
import PFL.Expr.LambdaLifted qualified as L
import PFL.Expr.Qualified qualified as Q
import Test.Sexp qualified as Sexp
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

type InExpr = CF.Cofree In.Expr ()

type QExpr = CF.Cofree (Q.Expr T.Text T.Text) ()

type LExpr = CF.Cofree (L.Expr T.Text T.Text) ()

main :: IO ()
main =
  defaultMain $
    testGroup
      "PFL"
      [ testQualify,
        testThread,
        testLinearise,
        testLambdaLift
      ]

testQualify :: TestTree
testQualify =
  testGroup
    "Qualify"
    [ testExpected
        "id"
        ["abs", "x", "x"]
        ["abs", "x", ["local", "x"]],
      testExpected
        "const global"
        ["abs", "x", "y"]
        ["abs", "x", ["global", "y"]],
      testExpected
        "Simple app"
        ["f", "x"]
        [["global", "f"], ["global", "x"]]
    ]
  where
    testExpected name inptStr expectedStr = testCase name $ do
      inpt :: InExpr <- Sexp.fromFail inptStr
      expected :: QExpr <- Sexp.fromFail expectedStr
      let outpt = qualify inpt
      case Sexp.unify (Sexp.into outpt) (Sexp.into expected) of
        Right () -> pure ()
        Left err -> fail (T.unpack err)

testThread :: TestTree
testThread =
  testGroup
    "Thread"
    [ testExpected
        "Simplest"
        ["threaded", ["!", "mx"]]
        ["abs", "0x", [["global", "mx"], ["local", "0x"]]]
    ]
  where
    testExpected name inptStr expectedStr = testCase name $ do
      inpt :: InExpr <- Sexp.fromFail inptStr
      expected :: QExpr <- Sexp.fromFail expectedStr
      let nms =
            Thread.Names
              { nmThreaded = "threaded",
                nmThread = "!",
                nmPair = "Pair"
              }
      let outpt = Thread.thread nms $ Q.anonymise $ qualify inpt
      case Sexp.unify (Sexp.into outpt) (Sexp.into expected) of
        Right () -> pure ()
        Left err -> fail (T.unpack err)

testLinearise :: TestTree
testLinearise =
  testGroup
    "Linearise"
    [ testExpected
        "Simplest copy"
        [["local", "x"], ["local", "x"]]
        [ "match",
          [["global", "copy"], ["local", "x"]],
          [["just", "Pair"], ["x", "0x"], [["local", "0x"], ["local", "x"]]]
        ],
      testExpected
        "Simplest drop"
        ["abs", "x", ["global", "y"]]
        [ "abs",
          "x",
          [ "match",
            [["global", "drop"], ["local", "x"]],
            [["just", "Unit"], [], ["global", "y"]]
          ]
        ],
      testExpected
        "Drop in branch"
        [ "match",
          ["local", "x"],
          [["just", "Foo"], [], ["local", "y"]],
          [["just", "Bar"], [], ["global", "z"]]
        ]
        [ "match",
          ["local", "x"],
          [["just", "Foo"], [], ["local", "y"]],
          [ ["just", "Bar"],
            [],
            [ "match",
              [["global", "drop"], ["local", "y"]],
              [["just", "Unit"], [], ["global", "z"]]
            ]
          ]
        ]
    ]
  where
    testExpected name inptStr expectedStr = testCase name $ do
      inpt :: QExpr <- Sexp.fromFail inptStr
      expected :: QExpr <- Sexp.fromFail expectedStr
      let nms =
            Linearise.Names
              { nmCopy = "copy",
                nmDrop = "drop",
                nmPair = "Pair",
                nmUnit = "Unit"
              }
      let outpt = Linearise.linearise nms $ Q.anonymise inpt
      case Sexp.unify (Sexp.into outpt) (Sexp.into expected) of
        Right () -> pure ()
        Left err -> fail (T.unpack err)

testLambdaLift :: TestTree
testLambdaLift =
  testGroup
    "Lambda Lift"
    [ testExpected
        "id"
        []
        ["abs", "x", ["local", "x"]]
        [ "closure",
          ["global", "Unit"],
          "x",
          [ "match",
            ["local", "x"],
            [ ["just", "Pair"],
              ["0x", "x"],
              [ "match",
                ["local", "0x"],
                [["just", "Unit"], [], ["local", "x"]]
              ]
            ]
          ]
        ],
      testExpected
        "Simple closure"
        [Q.Named "f"]
        ["abs", "x", [["local", "f"], ["local", "x"]]]
        [ "closure",
          ["local", "f"],
          "x",
          [ "match",
            ["local", "x"],
            [ ["just", "Pair"],
              ["f", "x"],
              [["local", "f"], ["local", "x"]]
            ]
          ]
        ],
      testExpected
        "Close-over 2"
        [Q.Named "y", Q.Named "f"]
        ["abs", "x", [[["local", "f"], ["local", "x"]], ["local", "y"]]]
        [ "closure",
          [[["global", "Pair"], ["local", "f"]], ["local", "y"]],
          "x",
          [ "match",
            ["local", "x"],
            [ ["just", "Pair"],
              ["0x", "x"],
              [ "match",
                ["local", "0x"],
                [ ["just", "Pair"],
                  ["f", "y"],
                  [[["local", "f"], ["local", "x"]], ["local", "y"]]
                ]
              ]
            ]
          ]
        ],
      testExpected
        "Close-over 3"
        [Q.Named "z", Q.Named "y", Q.Named "f"]
        ["abs", "x", [[[["local", "f"], ["local", "x"]], ["local", "y"]], ["local", "z"]]]
        [ "closure",
          [[["global", "Pair"], ["local", "f"]], [[["global", "Pair"], ["local", "y"]], ["local", "z"]]],
          "x",
          [ "match",
            ["local", "x"],
            [ ["just", "Pair"],
              ["0x", "x"],
              [ "match",
                ["local", "0x"],
                [ ["just", "Pair"],
                  ["f", "0x"],
                  [ "match",
                    ["local", "0x"],
                    [ ["just", "Pair"],
                      ["y", "z"],
                      [[[["local", "f"], ["local", "x"]], ["local", "y"]], ["local", "z"]]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ],
      closureOrderTest ("x", "y") ("x", "y"),
      closureOrderTest ("y", "x") ("x", "y"),
      closureOrderTest ("y", "x") ("y", "x")
    ]
  where
    testExpected name ctx inptStr expectedStr = testCase name $ do
      inpt :: QExpr <- Sexp.fromFail inptStr
      expected :: LExpr <- Sexp.fromFail expectedStr
      let nms =
            LambdaLift.Names
              { nmPair = "Pair",
                nmUnit = "Unit"
              }
      let outpt = flip runReader ctx $ LambdaLift.lambdaLift' nms $ Q.anonymise inpt
      case Sexp.unify (Sexp.into outpt) (Sexp.into expected) of
        Right () -> pure ()
        Left err -> fail (T.unpack err)

    closureOrderTest (x1, x2) (y1, y2) =
      testExpected
        ("Curry order: \\" <> show (x1, x2) <> " -> " <> show (y1, y2))
        []
        ["abs", x1, ["abs", x2, ["abs", "z", [[["local", y1], ["local", y2]], ["local", "z"]]]]]
        [ "closure",
          ["global", "Unit"],
          x1,
          [ "match",
            ["local", x1],
            [ ["just", "Pair"],
              ["0x", x1],
              [ "match",
                ["local", "0x"],
                [ ["just", "Unit"],
                  [],
                  [ "closure",
                    ["local", x1],
                    x2,
                    [ "match",
                      ["local", x2],
                      [ ["just", "Pair"],
                        [x1, x2],
                        [ "closure",
                          [[["global", "Pair"], ["local", x1]], ["local", x2]],
                          "z",
                          [ "match",
                            ["local", "z"],
                            [ ["just", "Pair"],
                              ["0x", "z"],
                              [ "match",
                                ["local", "0x"],
                                [ ["just", "Pair"],
                                  [x1, x2],
                                  [[["local", y1], ["local", y2]], ["local", "z"]]
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
          ]
        ]
