{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Compiler.CodegenTest where

import Data.ByteString.Lazy (LazyByteString)
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Encoding qualified as Text
import PyF
import Test.Tasty
import Test.Tasty.Golden (goldenVsStringDiff)

import Compiler.Codegen.X86_64
import Compiler.Parser.Parser (parseStatements, testParser)
import Compiler.Renamer (rename)
import Compiler.Types.Unique
import TestUtils

diffCmd :: String -> String -> [String]
diffCmd ref new = ["delta", "--diff-so-fancy", "--paging=never", ref, new]

specs :: TestTree
specs =
  testGroup
    "ASM Tests"
    [ goldenVsStringDiff
        "return 2"
        diffCmd
        "./test/golden/asm/return2.s"
        return2Test
    , goldenVsStringDiff
        "Bitwise 12"
        diffCmd
        "./test/golden/asm/bitwise12.s"
        bitwise12Test
    , goldenVsStringDiff
        "Negate 12"
        diffCmd
        "./test/golden/asm/negate12.s"
        negate12Test
    , goldenVsStringDiff
        "Logical negation"
        diffCmd
        "./test/golden/asm/logicalNegation.s"
        testLogicalNegation
    , goldenVsStringDiff
        "Addition"
        diffCmd
        "./test/golden/asm/addition.s"
        testAddition
    , goldenVsStringDiff
        "Subtraction"
        diffCmd
        "./test/golden/asm/subtraction.s"
        testSubtraction
    , goldenVsStringDiff
        "Division"
        diffCmd
        "./test/golden/asm/division.s"
        testDivision
    , goldenVsStringDiff
        "Lower Than Comparison"
        diffCmd
        "./test/golden/asm/lowerthan.s"
        testLowerThan
    , goldenVsStringDiff
        "logical operators"
        diffCmd
        "./test/golden/asm/logicalOperators.s"
        testLogicalOperators
    ]

return2Test :: IO LazyByteString
return2Test = do
  parsed <-
    assertParserRight $
      testParser
        parseStatements
        [str|
          int main() {
            return 2;
          }
        |]

  uniqueSupply <- mkUniqueSupply RenamingSection
  generated <- rename uniqueSupply parsed >>= assertRight >>= runCodeGen
  pure . Text.encodeUtf8 . Text.fromStrict $ generated

bitwise12Test :: IO LazyByteString
bitwise12Test = do
  parsed <-
    assertParserRight $
      testParser
        parseStatements
        [str|
          int main() {
            return ~12;
          }
        |]

  uniqueSupply <- mkUniqueSupply RenamingSection
  generated <- rename uniqueSupply parsed >>= assertRight >>= runCodeGen
  pure . Text.encodeUtf8 . Text.fromStrict $ generated

negate12Test :: IO LazyByteString
negate12Test = do
  parsed <-
    assertParserRight $
      testParser
        parseStatements
        [str|
          int main() {
            return -12;
          }
        |]

  uniqueSupply <- mkUniqueSupply RenamingSection
  generated <- rename uniqueSupply parsed >>= assertRight >>= runCodeGen
  pure . Text.encodeUtf8 . Text.fromStrict $ generated

testLogicalNegation :: IO LazyByteString
testLogicalNegation = do
  parsed <-
    assertParserRight $
      testParser
        parseStatements
        [str|
          int main() {
            return !1;
          }
        |]

  uniqueSupply <- mkUniqueSupply RenamingSection
  generated <- rename uniqueSupply parsed >>= assertRight >>= runCodeGen
  pure . Text.encodeUtf8 . Text.fromStrict $ generated

testAddition :: IO LazyByteString
testAddition = do
  parsed <-
    assertParserRight $
      testParser
        parseStatements
        [str|
            int main() {
              return 3 + 2;
            }
          |]

  uniqueSupply <- mkUniqueSupply RenamingSection
  generated <- rename uniqueSupply parsed >>= assertRight >>= runCodeGen
  pure . Text.encodeUtf8 . Text.fromStrict $ generated

testSubtraction :: IO LazyByteString
testSubtraction = do
  parsed <-
    assertParserRight $
      testParser
        parseStatements
        [str|
            int main() {
              return 3 - 2;
            }
          |]

  uniqueSupply <- mkUniqueSupply RenamingSection
  generated <- rename uniqueSupply parsed >>= assertRight >>= runCodeGen
  pure . Text.encodeUtf8 . Text.fromStrict $ generated

testDivision :: IO LazyByteString
testDivision = do
  parsed <-
    assertParserRight $
      testParser
        parseStatements
        [str|
            int main() {
              return 10 / 2;
            }
          |]

  uniqueSupply <- mkUniqueSupply RenamingSection
  generated <- rename uniqueSupply parsed >>= assertRight >>= runCodeGen
  pure . Text.encodeUtf8 . Text.fromStrict $ generated

testLowerThan :: IO LazyByteString
testLowerThan = do
  parsed <-
    assertParserRight $
      testParser
        parseStatements
        [str|
            int main() {
              return 10 < 2;
            }
          |]

  uniqueSupply <- mkUniqueSupply RenamingSection
  generated <- rename uniqueSupply parsed >>= assertRight >>= runCodeGen
  pure . Text.encodeUtf8 . Text.fromStrict $ generated

testLogicalOperators :: IO LazyByteString
testLogicalOperators = do
  parsed <-
    assertParserRight $
      testParser
        parseStatements
        [str|
            int main() {
              return 0 && 1;
            }
          |]

  uniqueSupply <- mkUniqueSupply RenamingSection
  generated <- rename uniqueSupply parsed >>= assertRight >>= runCodeGen
  pure . Text.encodeUtf8 . Text.fromStrict $ generated
