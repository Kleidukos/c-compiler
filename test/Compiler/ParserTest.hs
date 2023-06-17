{-# LANGUAGE OverloadedLists #-}

module Compiler.ParserTest where

import PyF
import Test.Tasty
import Test.Tasty.HUnit

import Compiler.Parser.Parser (parseExpression, parseStatements, testParser)
import Compiler.Types.AST
import TestUtils

specs :: TestTree
specs =
  testGroup
    "Parser Tests"
    [ testGroup "Stage 1" stage1Tests
    , testGroup "Stage 2" stage2Tests
    , testGroup "Stage 3" stage3Tests
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
    @?= Block [Fun "main" [] (Return (Lit (LitInt 100)))]

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
    @?= Block [Fun "main" [] (Return (Lit (LitInt 0)))]

testNoNewlines :: Assertion
testNoNewlines = do
  parsed <-
    assertParserRight $
      testParser
        parseStatements
        [str|int main(){return 0;}|]

  parsed
    @?= Block [Fun "main" [] (Return (Lit (LitInt 0)))]

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
    @?= "1:1:\nunexpected end of input\nexpecting \"}\"\n"

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
    @?= Block [Fun "main" [] (Return (BitwiseComplement (Lit (LitInt 12))))]

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
    @?= Block [Fun "main" [] (Return (Negate (Lit (LitInt 12))))]

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
