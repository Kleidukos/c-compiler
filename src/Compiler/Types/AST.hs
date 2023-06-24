module Compiler.Types.AST where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Vector (Vector)

data PlumeType name
  = VarType name
  | FunType (PlumeType name) (PlumeType name)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data PlumeExpr (name :: Type) -- type of name
  = Var name
  | Lit PlumeLit
  | App (PlumeExpr name) (PlumeExpr name)
  | Negate (PlumeExpr name)
  | BitwiseComplement (PlumeExpr name)
  | LogicalNegation (PlumeExpr name)
  | Addition (PlumeExpr name) (PlumeExpr name)
  | Multiplication (PlumeExpr name) (PlumeExpr name)
  | Division (PlumeExpr name) (PlumeExpr name)
  | Subtraction (PlumeExpr name) (PlumeExpr name)
  | And (PlumeExpr name) (PlumeExpr name)
  | Or (PlumeExpr name) (PlumeExpr name)
  | Equal (PlumeExpr name) (PlumeExpr name)
  | NotEqual (PlumeExpr name) (PlumeExpr name)
  | LessThan (PlumeExpr name) (PlumeExpr name)
  | LessThanOrEqual (PlumeExpr name) (PlumeExpr name)
  | GreaterThan (PlumeExpr name) (PlumeExpr name)
  | GreaterThanOrEqual (PlumeExpr name) (PlumeExpr name)
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data AST name
  = Let name (PlumeExpr name)
  | Return (PlumeExpr name)
  | Fun (PlumeType name) name (Vector (Pat name)) (AST name)
  | Block (Vector (AST name))
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

data PlumeLit
  = LitInt Integer
  | LitFloat Double
  | LitChar Char
  | LitString Text
  deriving (Eq, Ord, Show)

data Pat name
  = -- | A variable that gets filled, like the parameter of a function.
    PatternVar name
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)
