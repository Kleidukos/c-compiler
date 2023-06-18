module Compiler.Renamer where

import Data.Map.Strict (Map)
import Compiler.Types.Name

data RenamerEnv = RenamerEnv
  { bindings :: Map PlumeName ()
  }
