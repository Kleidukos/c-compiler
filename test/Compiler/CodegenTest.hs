{-# LANGUAGE OverloadedLists #-}

module Compiler.CodegenTest where

import Data.ByteString.Lazy (LazyByteString)
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Encoding qualified as Text
import PyF
import Test.Tasty
import Test.Tasty.Golden (goldenVsStringDiff)

import Compiler.Codegen.X86_64
import Compiler.Parser.Parser (parseStatements, testParser)
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

  pure . Text.encodeUtf8 . Text.fromStrict $ runCodegen parsed

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

  pure . Text.encodeUtf8 . Text.fromStrict $ runCodegen parsed

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

  pure . Text.encodeUtf8 . Text.fromStrict $ runCodegen parsed

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

  pure . Text.encodeUtf8 . Text.fromStrict $ runCodegen parsed
