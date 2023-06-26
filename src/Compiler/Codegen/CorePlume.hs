module PlumeCore where

import Compiler.Types.Name (PlumeName)
import Data.Text (Text)
import Data.Vector (Vector)

data CoreType = CoreType PlumeName
  deriving (Eq, Ord, Show)

data CoreLit
  = LitInt Integer
  | LitFloat Double
  | LitChar Char
  | LitString Text
  deriving (Eq, Ord, Show)

data CoreStatement
  = Let PlumeName CoreExpr
  | Return CoreExpr
  | Fun
      CoreType
      -- ^ Return type
      PlumeName
      -- ^ Name of the function
      (Vector PlumeName)
      -- ^ Parameters
      CoreStatement
      -- ^ Body of the function
  | IfThenElse
      CoreExpr
      -- ^ Conditional
      CoreStatement
      -- ^ If true
      CoreStatement
      -- ^ If false
  | Block (Vector CoreStatement)
  deriving stock (Eq, Ord, Show)

data CoreExpr -- type of name
  = Var PlumeName
  | Lit CoreLit
  | App CoreExpr CoreExpr
  | Negate CoreExpr
  | BitwiseComplement CoreExpr
  | LogicalNegation CoreExpr
  | Addition CoreExpr CoreExpr
  | Multiplication CoreExpr CoreExpr
  | Division CoreExpr CoreExpr
  | Subtraction CoreExpr CoreExpr
  | And CoreExpr CoreExpr
  | Or CoreExpr CoreExpr
  | Equal CoreExpr CoreExpr
  | NotEqual CoreExpr CoreExpr
  | LessThan CoreExpr CoreExpr
  | LessThanOrEqual CoreExpr CoreExpr
  | GreaterThan CoreExpr CoreExpr
  | GreaterThanOrEqual CoreExpr CoreExpr
  deriving stock (Eq, Ord, Show)
