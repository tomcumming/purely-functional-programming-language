module Theory.FunctionNotation (test) where

import Data.Foldable (traverse_)
import Data.String (IsString, fromString)
import Data.Text qualified as T
import Data.Text.IO qualified as T

data Ty
  = Con T.Text
  | Tuple [Ty]
  | Closure [Ty] Ty Ty
  deriving (Eq, Ord, Show)

instance IsString Ty where
  fromString = Con . T.pack

snoc :: [a] -> a -> [a]
snoc xs = (xs <>) . pure

apply :: Ty -> Ty -> Either T.Text Ty
apply = curry $ \case
  (Closure _ctxs reqArg ret, supArg)
    | reqArg == supArg -> pure ret
  (fn, arg) -> Left $ "Bad application: " <> T.show (fn, arg)

data ClosureNotation
  = ClosureNotation
      [Ty] -- Args
      Ty -- Return

closureNotation :: [Ty] -> Ty -> Ty -> ClosureNotation
closureNotation ctxs arg = \case
  Closure ctxs2 arg2 ret2
    | ctxs2 == snoc ctxs arg ->
        let ClosureNotation args ret3 = closureNotation ctxs2 arg2 ret2
         in ClosureNotation (arg : args) ret3
  ret -> ClosureNotation [arg] ret

prettyTuple :: [Ty] -> T.Text
prettyTuple ts =
  T.concat
    [ "[",
      T.intercalate ", " $ fmap pretty ts,
      "]"
    ]

pretty :: Ty -> T.Text
pretty = \case
  Con c -> c
  Tuple ts -> prettyTuple ts
  Closure ctxs arg ret ->
    let ClosureNotation args ret' = closureNotation ctxs arg ret
     in T.intercalate
          " "
          [ prettyTuple ctxs,
            "%",
            prettyTuple args,
            "->",
            pretty ret'
          ]

testFn :: Ty
testFn =
  Closure
    []
    "A"
    $ Closure
      ["A"]
      "B"
    $ Closure
      ["A", "B"]
      "C"
      "D"

test :: IO ()
test = traverse_ (go testFn . (`take` ["A", "B", "C"])) ([0 .. 3] :: [Int])
  where
    go :: Ty -> [Ty] -> IO ()
    go t = \case
      [] -> T.putStrLn $ pretty t
      (arg : args) -> do
        t' <- either (fail . T.unpack) pure $ apply t arg
        go t' args
