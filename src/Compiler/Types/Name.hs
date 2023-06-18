module Compiler.Types.Name where

import Data.Text (Text)

data PlumeName = PlumeName
  { nameSort :: !NameSort
  , name :: !Text
  }
  deriving stock (Eq, Ord, Show)

data NameSort
  = ModuleInternal
    -- ^ Defined in the module/file
  | ModuleExternal
    -- ^ Taken from outside of the module/file
  | System
    -- ^ Wired-in
  deriving stock (Eq, Ord, Show, Enum, Bounded)
