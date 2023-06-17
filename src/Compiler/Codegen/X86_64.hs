{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Compiler.Codegen.X86_64 where

import Compiler.Types.AST
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Utils.Output

runCodegen :: AST -> Text
runCodegen ast =
  let doc = emit ast <> "\n"
   in renderStrict $
        layoutPretty
          (defaultLayoutOptions{layoutPageWidth = Unbounded})
          doc

emit :: AST -> Doc ann
emit = \case
  Fun name _ body -> emitFunction name body
  Return expr -> emitReturn expr
  Block exprs -> emitBlock exprs

emitBlock :: Vector AST -> Doc ann
emitBlock stmts = Vector.foldMap' emit stmts

emitExpr :: PlumeExpr -> Doc ann
emitExpr = \case
  Lit lit -> emitLiteral lit
  Negate expr -> emitNegate expr
  BitwiseComplement expr -> emitBitwiseComplement expr
  LogicalNegation expr -> emitLogicalNegation expr
  Addition leftExpr rightExpr -> emitAddition leftExpr rightExpr
  Multiplication leftExpr rightExpr -> emitMultiplication leftExpr rightExpr
  Division leftExpr rightExpr -> emitDivision leftExpr rightExpr
  Subtraction leftExpr rightExpr -> emitSubtraction leftExpr rightExpr

emitLiteral :: PlumeLit -> Doc ann
emitLiteral = \case
  LitInt i -> emitNumber i

-- PlumeVar t ->
-- PlumeApp PlumeExpr PlumeExpr

emitNumber :: Integer -> Doc ann
emitNumber i = "movq" <×> "$" <> pretty i <> ", %rax"

emitReturn :: PlumeExpr -> Doc ann
emitReturn expr =
  let body = emitExpr expr
   in vcat
        [ body
        , "ret"
        ]

emitFunction :: Text -> AST -> Doc ann
emitFunction name stmt =
  vcat
    [ indent 4 ".globl " <> pretty functionName
    , pretty functionName <> ":"
    , indent 4 result
    ]
  where
    result = emit stmt
    functionName =
      if name == "main"
        then "main"
        else "_" <> name

emitNegate :: PlumeExpr -> Doc ann
emitNegate expr =
  vcat
    [ body
    , "neg" <×> "%rax"
    ]
  where
    body = emitExpr expr

emitBitwiseComplement :: PlumeExpr -> Doc ann
emitBitwiseComplement expr =
  vcat
    [ body
    , "not" <×> "%rax"
    ]
  where
    body = emitExpr expr
emitLogicalNegation :: PlumeExpr -> Doc ann
emitLogicalNegation expr =
  vcat
    [ body
    , "cmpq" <×> "$0, %rax"
    , "movq" <×> "$0, %rax"
    , "sete" <×> "%al"
    ]
  where
    body = emitExpr expr

emitAddition :: PlumeExpr -> PlumeExpr -> Doc ann
emitAddition leftExpr rightExpr =
  vcat
    [ "# left operand"
    , leftBody
    , "push" <×> "%rax # save value of left operand on the stack"
    , "# right operand"
    , rightBody
    , "pop" <×> "%rcx # pop left operand from the stack into %rcx"
    , "addq" <×> "%rcx, %rax # add left operand to right operand, save result in %rax"
    ]
  where
    leftBody = emitExpr leftExpr
    rightBody = emitExpr rightExpr

emitMultiplication :: PlumeExpr -> PlumeExpr -> Doc ann
emitMultiplication leftExpr rightExpr = do
  vcat
    [ "# left operand"
    , leftBody
    , "push" <×> "%rax # save value of left operand on the stack"
    , "# right operand"
    , rightBody
    , "pop" <×> "%rcx # pop left operand from the stack into %rcx"
    , "imuq" <×> "%rcx, %rax # multiply left operand by right operand, save result in %rax"
    ]
  where
    leftBody = emitExpr leftExpr
    rightBody = emitExpr rightExpr

emitSubtraction :: PlumeExpr -> PlumeExpr -> Doc ann
emitSubtraction leftExpr rightExpr = do
  vcat
    [ "# right operand"
    , rightBody
    , "push" <×> "%rax # save value of right operand on the stack"
    , "# left operand"
    , leftBody
    , "pop" <×> "%rcx # pop right operand from the stack into %rcx"
    , "subq" <×> "%rcx, %rax # subtract right from left (that is in %rax), save result in %rax"
    ]
  where
    leftBody = emitExpr leftExpr
    rightBody = emitExpr rightExpr

emitDivision :: PlumeExpr -> PlumeExpr -> Doc ann
emitDivision leftExpr rightExpr = do
  vcat
    [ "# right operand"
    , rightBody
    , "push" <×> "%rax # save value of right operand on the stack"
    , "# left operand"
    , leftBody
    , "pop" <×> "%rcx # pop right operand from the stack into %rcx"
    , "cdq"
    , "idivq" <×> "%rcx # divide left by right (that is in %rax), save result in %rax"
    ]
  where
    leftBody = emitExpr leftExpr
    rightBody = emitExpr rightExpr
