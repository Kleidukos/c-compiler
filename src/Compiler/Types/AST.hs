module Compiler.Types.AST where

import Data.Text (Text)
import Data.Vector (Vector)

import Compiler.Types.Name

data PlumeType
  = VarType PlumeName
  | FunType PlumeType PlumeType
  deriving (Eq, Ord, Show)

data PlumeExpr
  = Var PlumeName
  | Lit PlumeLit
  | App PlumeExpr PlumeExpr
  | Negate PlumeExpr
  | BitwiseComplement PlumeExpr
  | LogicalNegation PlumeExpr
  | Addition PlumeExpr PlumeExpr
  | Multiplication PlumeExpr PlumeExpr
  | Division PlumeExpr PlumeExpr
  | Subtraction PlumeExpr PlumeExpr
  | And PlumeExpr PlumeExpr
  | Or PlumeExpr PlumeExpr
  | Equal PlumeExpr PlumeExpr
  | NotEqual PlumeExpr PlumeExpr
  | LessThan PlumeExpr PlumeExpr
  | LessThanOrEqual PlumeExpr PlumeExpr
  | GreaterThan PlumeExpr PlumeExpr
  | GreaterThanOrEqual PlumeExpr PlumeExpr
  deriving (Eq, Ord, Show)

data AST
  = Let Text PlumeExpr
  | Return PlumeExpr
  | Fun PlumeType (Vector Pat) AST
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
