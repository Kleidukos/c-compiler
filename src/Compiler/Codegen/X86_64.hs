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
  PlumeLit lit -> emitLiteral lit
  PlumeNegate expr -> emitNegate expr
  PlumeBitwiseComplement expr -> emitBitwiseComplement expr

emitLiteral :: PlumeLit -> Doc ann
emitLiteral = \case
  LitInt i -> emitNumber i

-- PlumeVar t ->
-- PlumeApp PlumeExpr PlumeExpr

emitNumber :: Integer -> Doc ann
emitNumber i = "movl" <×> "$" <> pretty i <> ", %eax"

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
    , "neg" <×> "%eax"
    ]
  where
    body = emitExpr expr

emitBitwiseComplement :: PlumeExpr -> Doc ann
emitBitwiseComplement expr =
  vcat
    [ body
    , "not" <×> "%eax"
    ]
  where
    body = emitExpr expr

emitLogicalNegation :: PlumeExpr -> Doc ann
emitLogicalNegation expr =
  vcat
    [ body
    , "cmpl" <×> "$0, %eax"
    , "movl" <×> "$0, %eax"
    , "sete" <×> "%al"
    ]
  where
    body = emitExpr expr
