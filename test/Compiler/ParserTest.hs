{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Compiler.ParserTest where

import PyF
import Test.Tasty
import Test.Tasty.HUnit

import Compiler.Parser.Parser (parseExpression, parseStatements, testParser)
import Compiler.Types.AST
import Compiler.Types.Name
import TestUtils

specs :: TestTree
specs =
  testGroup
    "Parser Tests"
    [ testGroup "Stage 1" stage1Tests
    , testGroup "Stage 2" stage2Tests
    , testGroup "Stage 3" stage3Tests
    , testGroup "Stage 4" stage4Tests
    , testGroup "Stage 5" stage5Tests
    , testGroup "Stage 6" stage6Tests
    ]

stage1Tests :: [TestTree]
stage1Tests =
  [ testCase "Multi-digit return" testMultiDigitReturn
  , testCase "Bunch of newlines" testBunchOfNewlines
  , testCase "No newlines" testNoNewlines
  , testCase "Missing closing paren" testMissingClosingParen
  , testCase "Missing return value" testMissingReturnValue
  , testCase "Missing closing brace" testMissingClosingBrace
  ]

testMultiDigitReturn :: Assertion
testMultiDigitReturn = do
  parsed <-
    assertParserRight $
      testParser
        parseStatements
        [str| 
        int main() {
          return 100;
        }
        |]

  parsed
    @?= Block
      [ Fun
          ( VarType
              (CoreName{namespace = TypeConstructor, nameText = "int"})
          )
          (CoreName{namespace = Binding, nameText = "main"})
          []
          (Block [Return (Lit (LitInt 100))])
      ]
testBunchOfNewlines :: Assertion
testBunchOfNewlines = do
  parsed <-
    assertParserRight $
      testParser
        parseStatements
        [str| 
        int 
        main
        (   
        )
        {
        return
        0
        ;
        }
        |]

  parsed
    @?= Block
      [ Fun
          (VarType (CoreName{namespace = TypeConstructor, nameText = "int"}))
          (CoreName{namespace = Binding, nameText = "main"})
          []
          (Block [Return (Lit (LitInt 0))])
      ]

testNoNewlines :: Assertion
testNoNewlines = do
  parsed <-
    assertParserRight $
      testParser
        parseStatements
        [str|int main(){return 0;}|]

  parsed
    @?= Block
      [ Fun
          (VarType (CoreName{namespace = TypeConstructor, nameText = "int"}))
          (CoreName{namespace = Binding, nameText = "main"})
          []
          (Block [Return (Lit (LitInt 0))])
      ]

testMissingClosingParen :: Assertion
testMissingClosingParen = do
  parsed <-
    assertParserLeft $
      testParser
        parseStatements
        [str|
        int main( {
            return 0;
        }
        |]

  parsed @?= "1:1:\n  |\n1 |         int main( {\n  | ^\nunexpected {\nexpecting \")\"\n"

testMissingReturnValue :: Assertion
testMissingReturnValue = do
  parsed <-
    assertParserLeft $
      testParser
        parseStatements
        [str|
        int main() {
            return;
        }
        |]

  parsed @?= "1:1:\n  |\n1 |             return;\n  | ^\nunexpected ;\nexpecting Expression\n"

testMissingClosingBrace :: Assertion
testMissingClosingBrace = do
  parsed <-
    assertParserLeft $
      testParser
        parseStatements
        [str|
        int main() {
            return 0;
        |]

  parsed
    @?= "1:1:\nunexpected end of input\nexpecting \"}\", Assignment, Function, If-Then-Else, or Return\n"

stage2Tests :: [TestTree]
stage2Tests =
  [ testCase "Parse bitwise complement" testParseBitwise
  , testCase "Parse arithmetic negation" testParseArithmeticNegation
  , testCase "Nested missing constant" testParseNestedMissingConstant
  , testCase "Prefix operation in postfix order" testPrefixOperationInPostfixOrder
  ]

testParseBitwise :: Assertion
testParseBitwise = do
  parsed <-
    assertParserRight $
      testParser
        parseStatements
        [str|
        int main() {
            return ~12;
        }
        |]

  parsed
    @?= Block
      [ Fun
          (VarType (CoreName{namespace = TypeConstructor, nameText = "int"}))
          (CoreName{namespace = Binding, nameText = "main"})
          []
          ( Block [Return (BitwiseComplement (Lit (LitInt 12)))]
          )
      ]

testParseArithmeticNegation :: Assertion
testParseArithmeticNegation = do
  parsed <-
    assertParserRight $
      testParser
        parseStatements
        [str|
        int main() {
            return -12;
        }
        |]

  parsed
    @?= Block
      [ Fun
          (VarType (CoreName{namespace = TypeConstructor, nameText = "int"}))
          (CoreName{namespace = Binding, nameText = "main"})
          []
          ( Block
              [ Return (Negate (Lit (LitInt 12)))
              ]
          )
      ]

testParseNestedMissingConstant :: Assertion
testParseNestedMissingConstant = do
  parsed <-
    assertParserLeft $
      testParser
        parseStatements
        [str|
        int main() {
            return !~;
        }
        |]

  parsed
    @?= "1:1:\n  |\n1 |             return !~;\n  | ^\nunexpected !~\nexpecting Expression\n"

testPrefixOperationInPostfixOrder :: Assertion
testPrefixOperationInPostfixOrder = do
  parsed <-
    assertParserLeft $
      testParser
        parseStatements
        [str|
        int main() {
            return 4-;
        }
        |]

  parsed
    @?= "1:1:\n  |\n1 |             return 4-;\n  | ^\nunexpected ;\nexpecting \"!\", \"-\", \"~\", or term\n"

stage3Tests :: [TestTree]
stage3Tests =
  [ testCase "Parse addition" parseAddition
  , testCase "Parse subtraction" parseSubtraction
  , testCase "Parse mixed operators" parseMixedOperators
  ]

parseAddition :: Assertion
parseAddition = do
  parsed <-
    assertParserRight $
      testParser
        parseExpression
        [str| 2 + 3 * 4 |]

  parsed
    @?= Addition (Lit (LitInt 2)) (Multiplication (Lit (LitInt 3)) (Lit (LitInt 4)))

parseSubtraction :: Assertion
parseSubtraction = do
  parsed <-
    assertParserRight $
      testParser
        parseExpression
        [str| (2 - 4) |]

  parsed
    @?= Subtraction (Lit (LitInt 2)) (Lit (LitInt 4))

parseMixedOperators :: Assertion
parseMixedOperators = do
  parsed <-
    assertParserRight $
      testParser
        parseExpression
        [str| ~2 + 4 |]

  parsed
    @?= Addition (BitwiseComplement (Lit (LitInt 2))) (Lit (LitInt 4))

stage4Tests :: [TestTree]
stage4Tests =
  [ testCase "Binary comparison" testParseBinaryComparison
  , testCase "Boolean AND and OR" testParseBooleanLogic
  , testCase "Chained operators" testChainedOperators
  ]

testParseBinaryComparison :: Assertion
testParseBinaryComparison = do
  parsed <-
    assertParserRight $
      testParser
        parseExpression
        [str| (~2 + 4) >= 2 |]

  parsed
    @?= GreaterThanOrEqual (Addition (BitwiseComplement (Lit (LitInt 2))) (Lit (LitInt 4))) (Lit (LitInt 2))

testParseBooleanLogic :: Assertion
testParseBooleanLogic = do
  parsed <-
    assertParserRight $
      testParser
        parseExpression
        [str| (0 && 1) || 1 |]

  parsed
    @?= Or (And (Lit (LitInt 0)) (Lit (LitInt 1))) (Lit (LitInt 1))

testChainedOperators :: Assertion
testChainedOperators = do
  parsed <-
    assertParserRight $
      testParser
        parseStatements
        [str|
        int main() {
            return 1 || 0 && 2;
        }
        |]

  parsed
    @?= Block
      [ Fun
          ( VarType
              (CoreName{namespace = TypeConstructor, nameText = "int"})
          )
          (CoreName{namespace = Binding, nameText = "main"})
          []
          (Block [Return (And (Or (Lit (LitInt 1)) (Lit (LitInt 0))) (Lit (LitInt 2)))])
      ]

stage5Tests :: [TestTree]
stage5Tests =
  [ testCase "Parse declaration and assignment" parseAssignment
  ]

parseAssignment :: Assertion
parseAssignment = do
  parsed <-
    assertParserRight $
      testParser
        parseStatements
        [str|
        int main() {
            int a = 3;
            return a * 2;
        }
        |]

  parsed
    @?= Block
      [ Fun
          (VarType (CoreName{namespace = TypeConstructor, nameText = "int"}))
          (CoreName{namespace = Binding, nameText = "main"})
          []
          ( Block
              [ Let (CoreName{namespace = Binding, nameText = "a"}) (Lit (LitInt 3))
              , Return
                  ( Multiplication
                      ( Var
                          (CoreName{namespace = Binding, nameText = "a"})
                      )
                      (Lit (LitInt 2))
                  )
              ]
          )
      ]

stage6Tests :: [TestTree]
stage6Tests =
  [ testCase "Parse if-then-else" parseIfThenElse
  ]

parseIfThenElse :: Assertion
parseIfThenElse = do
  parsed <-
    assertParserRight $
      testParser
        parseStatements
        [str|
        int main() {
            int a = 3;
            if (a < 4) 
            then 
              return a * 2;
            else
              return a / 3;
        }
        |]

  parsed
    @?= Block
      [ Fun
          (VarType (CoreName{namespace = TypeConstructor, nameText = "int"}))
          (CoreName{namespace = Binding, nameText = "main"})
          []
          ( Block
              [ Let (CoreName{namespace = Binding, nameText = "a"}) (Lit (LitInt 3))
              , IfThenElse
                  ( LessThan
                      (Var (CoreName{namespace = Binding, nameText = "a"}))
                      (Lit (LitInt 4))
                  )
                  ( Block
                      [ Return
                          ( Multiplication
                              (Var (CoreName{namespace = Binding, nameText = "a"}))
                              (Lit (LitInt 2))
                          )
                      ]
                  )
                  ( Block
                      [ Return
                          ( Division
                              (Var (CoreName{namespace = Binding, nameText = "a"}))
                              (Lit (LitInt 3))
                          )
                      ]
                  )
              ]
          )
      ]
