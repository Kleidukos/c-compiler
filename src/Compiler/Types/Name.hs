module Compiler.Types.Name where

import Compiler.Types.Unique
import Data.Text (Text)

-- | This datatype is at the core of every other name.
data CoreName = CoreName
  { namespace :: !Namespace
  , nameText :: !Text
  }
  deriving stock (Eq, Ord, Show)

-- | We get this kind of name after renaming.
data PlumeName = PlumeName
  { nameSort :: !NameSort
  , coreName :: !CoreName
  , unique :: !Unique
  }
  deriving stock (Show)

instance Eq PlumeName where
  (==) n1 n2 = n1.unique == n2.unique

instance Ord PlumeName where
  compare n1 n2 = compare n1.unique n2.unique

data NameSort
  = -- | Defined in the module/file
    ModuleInternal
  | -- | Taken from outside of the module/file
    ModuleExternal
  | -- | Wired-in
    System
  deriving stock (Eq, Ord, Show, Enum, Bounded)

--- | To what kind of namespace does the name belongs: bindings or data constructors
data Namespace
  = -- | variables
    Binding
  | -- | term-level data constructors
    DataConstructor
  | -- | Type constructors
    TypeConstructor
  deriving stock (Eq, Ord, Show)
