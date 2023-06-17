module Compiler.Types.AST where

import Data.Text (Text)
import Data.Vector (Vector)

import Compiler.Types.Name

data PlumeType
  = VarType PlumeName
  | FunType PlumeType PlumeType
  deriving (Eq, Ord, Show)

data PlumeExpr
  = Var Text
  | Lit PlumeLit
  | App PlumeExpr PlumeExpr
  | Negate PlumeExpr
  | BitwiseComplement PlumeExpr
  | LogicalNegation PlumeExpr
  | Addition PlumeExpr PlumeExpr
  | Multiplication PlumeExpr PlumeExpr
  | Division PlumeExpr PlumeExpr
  | Subtraction PlumeExpr PlumeExpr
  deriving (Eq, Ord, Show)

data AST
  = Let PlumeExpr
  | Return PlumeExpr
  | Fun Text (Vector Pat) AST
  | Block (Vector AST)
  deriving (Eq, Ord, Show)

data PlumeLit
  = LitInt Integer
  | LitFloat Double
  | LitChar Char
  | LitString Text
  deriving (Eq, Ord, Show)

data Pat
  = PatternVar Text
  deriving (Eq, Ord, Show)
