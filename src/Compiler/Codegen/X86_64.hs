{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Compiler.Codegen.X86_64 where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Compiler.Types.AST
import Control.Concurrent
import Data.Function
import Effectful
import Effectful.Reader.Static (Reader, runReader)
import Utils.Output

data CodeGenEnv = CodeGenEnv
  { labelCounter :: Word
  }
  deriving stock (Eq, Ord, Show)

type CodeGenM a = Eff '[Reader (MVar CodeGenEnv), IOE] a

data PlumeOrdering
  = PlumeGT
  | PlumeGTE
  | PlumeLT
  | PlumeLTE
  | PlumeEQ
  | PlumeNE
  deriving stock (Eq, Ord, Show)

runCodeGen :: AST -> IO Text
runCodeGen ast = do
  env <- newCodeGenEnv
  let action = emit ast
  doc <-
    action
      & runReader env
      & runEff
  pure $
    renderStrict $
      layoutPretty
        (defaultLayoutOptions{layoutPageWidth = Unbounded})
        (doc <> "\n")

newCodeGenEnv :: IO (MVar CodeGenEnv)
newCodeGenEnv = newMVar (CodeGenEnv{labelCounter = 0})

emit :: AST -> CodeGenM (Doc ann)
emit = \case
  Fun name _ body -> emitFunction name body
  Return expr -> emitReturn expr
  Block exprs -> emitBlock exprs

emitBlock :: Vector AST -> CodeGenM (Doc ann)
emitBlock stmts = Vector.foldMap' emit stmts

emitExpr :: PlumeExpr -> CodeGenM (Doc ann)
emitExpr = \case
  Lit lit -> emitLiteral lit
  Negate expr -> emitNegate expr
  BitwiseComplement expr -> emitBitwiseComplement expr
  LogicalNegation expr -> emitLogicalNegation expr
  Addition leftExpr rightExpr -> emitAddition leftExpr rightExpr
  Multiplication leftExpr rightExpr -> emitMultiplication leftExpr rightExpr
  Division leftExpr rightExpr -> emitDivision leftExpr rightExpr
  Subtraction leftExpr rightExpr -> emitSubtraction leftExpr rightExpr
  LessThan leftExpr rightExpr -> emitComparison PlumeLT leftExpr rightExpr
  LessThanOrEqual leftExpr rightExpr -> emitComparison PlumeLTE leftExpr rightExpr
  GreaterThan leftExpr rightExpr -> emitComparison PlumeGT leftExpr rightExpr
  GreaterThanOrEqual leftExpr rightExpr -> emitComparison PlumeGTE leftExpr rightExpr

emitLiteral :: PlumeLit -> CodeGenM (Doc ann)
emitLiteral = \case
  LitInt i -> emitNumber i

-- PlumeVar t ->
-- PlumeApp PlumeExpr PlumeExpr

emitNumber :: Integer -> CodeGenM (Doc ann)
emitNumber i = pure $ "movq" <×> "$" <> pretty i <> ", %rax"

emitReturn :: PlumeExpr -> CodeGenM (Doc ann)
emitReturn expr = do
  (body :: Doc ann) <- emitExpr expr
  pure $
    vcat
      [ body
      , "ret"
      ]

emitFunction :: Text -> AST -> CodeGenM (Doc ann)
emitFunction name stmt = do
  result <- emit stmt
  pure $
    vcat
      [ indent 4 ".globl " <> pretty functionName
      , pretty functionName <> ":"
      , indent 4 result
      ]
  where
    functionName = do
      if name == "main"
        then "main"
        else "_" <> name

emitNegate :: PlumeExpr -> CodeGenM (Doc ann)
emitNegate expr = do
  body <- emitExpr expr
  pure $
    vcat
      [ body
      , "neg" <×> "%rax"
      ]

emitBitwiseComplement :: PlumeExpr -> CodeGenM (Doc ann)
emitBitwiseComplement expr = do
  body <- emitExpr expr
  pure $
    vcat
      [ body
      , "not" <×> "%rax"
      ]

emitLogicalNegation :: PlumeExpr -> CodeGenM (Doc ann)
emitLogicalNegation expr = do
  body <- emitExpr expr
  pure $
    vcat
      [ body
      , "cmpq" <×> "$0, %rax"
      , "movq" <×> "$0, %rax"
      , "sete" <×> "%al"
      ]

emitAddition :: PlumeExpr -> PlumeExpr -> CodeGenM (Doc ann)
emitAddition leftExpr rightExpr = do
  leftBody <- emitExpr leftExpr
  rightBody <- emitExpr rightExpr
  pure $
    vcat
      [ "# left operand"
      , leftBody
      , "push" <×> "%rax # save value of left operand on the stack"
      , "# right operand"
      , rightBody
      , "pop " <×> "%rcx # pop left operand from the stack into %rcx"
      , "addq" <×> "%rcx, %rax # add left operand to right operand, save result in %rax"
      ]

emitMultiplication :: PlumeExpr -> PlumeExpr -> CodeGenM (Doc ann)
emitMultiplication leftExpr rightExpr = do
  leftBody <- emitExpr leftExpr
  rightBody <- emitExpr rightExpr
  pure $
    vcat
      [ "# left operand"
      , leftBody
      , "push" <×> "%rax # save value of left operand on the stack"
      , "# right operand"
      , rightBody
      , "pop " <×> "%rcx # pop left operand from the stack into %rcx"
      , "imuq" <×> "%rcx, %rax # multiply left operand by right operand, save result in %rax"
      ]

emitSubtraction :: PlumeExpr -> PlumeExpr -> CodeGenM (Doc ann)
emitSubtraction leftExpr rightExpr = do
  leftBody <- emitExpr leftExpr
  rightBody <- emitExpr rightExpr
  pure $
    vcat
      [ "# right operand"
      , rightBody
      , "push" <×> "%rax # save value of right operand on the stack"
      , "# left operand"
      , leftBody
      , "pop " <×> "%rcx # pop right operand from the stack into %rcx"
      , "subq" <×> "%rcx, %rax # subtract right from left (that is in %rax), save result in %rax"
      ]

emitDivision :: PlumeExpr -> PlumeExpr -> CodeGenM (Doc ann)
emitDivision leftExpr rightExpr = do
  leftBody <- emitExpr leftExpr
  rightBody <- emitExpr rightExpr
  pure $
    vcat
      [ "# right operand"
      , rightBody
      , "push" <×> "%rax # save value of right operand on the stack"
      , "# left operand"
      , leftBody
      , "pop " <×> "%rcx # pop right operand from the stack into %rcx"
      , "cdq"
      , "idivq" <×> "%rcx # divide left by right (that is in %rax), save result in %rax"
      ]

emitComparison :: PlumeOrdering -> PlumeExpr -> PlumeExpr -> CodeGenM (Doc ann)
emitComparison ordering leftExpr rightExpr = do
  leftBody <- emitExpr leftExpr
  rightBody <- emitExpr rightExpr
  pure $
    vcat
      [ "# left operand"
      , leftBody
      , "push" <×> "%rax"
      , "# right operand"
      , rightBody
      , "pop " <×> "%rcx # pop left from the stack into %rcx, right is already in %eax"
      , "cmpq" <×> "%rax, %rcx # set ZF on, if left == right, set if off otherwise"
      , "movq" <×> "$0, %rax # zero out %rax"
      , op <×> "%al # set %al (lower byte of %rax) to 1 iff ZF is on"
      ]
  where
    op = case ordering of
      PlumeGT -> "setg"
      PlumeGTE -> "setge"
      PlumeLT -> "setl"
      PlumeLTE -> "setle"
      PlumeEQ -> "sete"
      PlumeNE -> "setne"
