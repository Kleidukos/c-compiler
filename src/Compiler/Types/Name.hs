module Compiler.Types.Name where

import Data.Text (Text)

data PlumeName = PlumeName
  { nameSort :: !NameSort
  , name :: !Text
  }
  deriving stock (Eq, Ord, Show)

data NameSort
  = ModuleInternal
  | ModuleExternal
  | System
  deriving stock (Eq, Ord, Show, Enum, Bounded)
