module PFL.Typing.Kind (Kind (..)) where

data Kind
  = -- | A value (on the stack)
    Type
  | Arr Kind Kind
  deriving (Eq, Ord, Show)
