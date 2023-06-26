module Compiler.Codegen.X86_64 where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Compiler.Types.AST
import Compiler.Types.Name
import Compiler.Types.Unique
import Control.Concurrent
import Data.Function
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Utils.Output

data CodeGenEnv = CodeGenEnv
  { labelCounter :: Word
  }
  deriving stock (Eq, Ord, Show)

type CodeGenM a = Eff '[Reader UniqueSupply, IOE] a

data PlumeOrdering
  = PlumeGT
  | PlumeGTE
  | PlumeLT
  | PlumeLTE
  | PlumeEQ
  | PlumeNE
  deriving stock (Eq, Ord, Show)

data LogicalOperator
  = PlumeOR
  | PlumeAND
  deriving stock (Eq, Ord, Show)

runCodeGen :: AST PlumeName -> IO Text
runCodeGen ast = do
  uniqueSupply <- mkUniqueSupply CodeGenSection
  let action = emit ast
  doc <-
    action
      & Reader.runReader uniqueSupply
      & runEff
  pure $
    renderStrict $
      layoutPretty
        (defaultLayoutOptions{layoutPageWidth = Unbounded})
        (doc <> "\n")

newCodeGenEnv :: IO (MVar CodeGenEnv)
newCodeGenEnv = newMVar (CodeGenEnv{labelCounter = 0})

getNextLabel :: CodeGenM (Doc ann)
getNextLabel = do
  uniqueSupply <- Reader.ask
  unique <- liftIO $ nextUnique uniqueSupply
  pure $ ".L" <> prettyUnique unique

emit :: AST PlumeName -> CodeGenM (Doc ann)
emit = \case
  Fun _returnType name _patterns body -> emitFunction name body
  Return expr -> emitReturn expr
  Block exprs -> emitBlock exprs
  _ -> undefined

emitBlock :: Vector (AST PlumeName) -> CodeGenM (Doc ann)
emitBlock = Vector.foldMap' emit

emitExpr :: PlumeExpr PlumeName -> CodeGenM (Doc ann)
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
  And leftExpr rightExpr -> emitLogicalOperator PlumeAND leftExpr rightExpr
  Or leftExpr rightExpr -> emitLogicalOperator PlumeOR leftExpr rightExpr
  _ -> undefined

emitLiteral :: PlumeLit -> CodeGenM (Doc ann)
emitLiteral = \case
  LitInt i -> emitNumber i
  _ -> undefined

emitNumber :: Integer -> CodeGenM (Doc ann)
emitNumber i = pure $ "movq" <×> "$" <> pretty i <> ", %rax"

emitReturn :: PlumeExpr PlumeName -> CodeGenM (Doc ann)
emitReturn expr = do
  let epilogue =
        vcat
          [ "# Function epilogue"
          , "movq" <×> "%rbp, %rsp # Restore %rsp; now it points to the old %rbp"
          , "pop" <×> "%rbp # Restore old %rbp; now %rsp is where it was before the prologue"
          ]
  (body :: Doc ann) <- emitExpr expr
  pure $
    vcat
      [ body
      , epilogue
      , "ret"
      ]

emitFunction :: PlumeName -> AST PlumeName -> CodeGenM (Doc ann)
emitFunction plumeName stmt = do
  let prologue =
        indent 4 $
          vcat
            [ "# Function prologue"
            , "push" <×> "%rbp # Save old value of %rbp on the stack"
            , "movq" <×> "%rsp, %rbp # current top of the stack is the bottom of the new stack frame"
            ]
  result <- emit stmt
  pure $
    vcat
      [ indent 4 ".globl " <> pretty functionName
      , pretty functionName <> ":"
      , prologue
      , indent 4 result
      ]
  where
    name = plumeName.coreName.nameText
    functionName =
      if name == "main"
        then "main"
        else "_" <> name

emitNegate :: PlumeExpr PlumeName -> CodeGenM (Doc ann)
emitNegate expr = do
  body <- emitExpr expr
  pure $
    vcat
      [ body
      , "neg" <×> "%rax"
      ]

emitBitwiseComplement :: PlumeExpr PlumeName -> CodeGenM (Doc ann)
emitBitwiseComplement expr = do
  body <- emitExpr expr
  pure $
    vcat
      [ body
      , "not" <×> "%rax"
      ]

emitLogicalNegation :: PlumeExpr PlumeName -> CodeGenM (Doc ann)
emitLogicalNegation expr = do
  body <- emitExpr expr
  pure $
    vcat
      [ body
      , "cmpq" <×> "$0, %rax"
      , "movq" <×> "$0, %rax"
      , "sete" <×> "%al"
      ]

emitAddition :: PlumeExpr PlumeName -> PlumeExpr PlumeName -> CodeGenM (Doc ann)
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

emitMultiplication :: PlumeExpr PlumeName -> PlumeExpr PlumeName -> CodeGenM (Doc ann)
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

emitSubtraction :: PlumeExpr PlumeName -> PlumeExpr PlumeName -> CodeGenM (Doc ann)
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

emitDivision :: PlumeExpr PlumeName -> PlumeExpr PlumeName -> CodeGenM (Doc ann)
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

emitComparison :: PlumeOrdering -> PlumeExpr PlumeName -> PlumeExpr PlumeName -> CodeGenM (Doc ann)
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

emitLogicalOperator :: LogicalOperator -> PlumeExpr PlumeName -> PlumeExpr PlumeName -> CodeGenM (Doc ann)
emitLogicalOperator op leftExpr rightExpr = do
  leftBody <- emitExpr leftExpr
  rightBody <- emitExpr rightExpr
  clause2Label <- getNextLabel
  endLabel <- getNextLabel
  let logicalCode = case op of
        PlumeOR -> vcat ["je" <×> clause2Label, "movq" <×> "$1, %rax"]
        PlumeAND -> vcat ["jne" <×> clause2Label]

  pure $
    vsep
      [ vcat
          [ "# left operand"
          , leftBody
          , "cmpq" <×> "$0, %rax"
          , logicalCode
          ]
      , hang (-4) $
          vcat
            [ "jmp" <×> endLabel <> " # jump to end label"
            , clause2Label <> ":"
            ]
      , vcat
          [ "# right operand"
          , rightBody
          , "cmpq" <×> "$0, %rax # check if right expr is true"
          , "movq" <×> "$0, %rax # zero out %rax"
          ]
      , hang (-4) $
          vcat
            [ "setne" <×> "%al # set %al (low byte of %rax) to 1 iff right expr is true"
            , endLabel <> ":" <×> " # end label"
            ]
      ]
