{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Compiler.RenamerTest where

import PyF
import Test.Tasty
import Test.Tasty.HUnit

import Compiler.Parser.Parser (parseStatements, testParser)
import Compiler.Renamer (rename, RenamingError(..))
import Compiler.Types.Unique (UniqueSection (..), mkUniqueSupply)
import TestUtils

specs :: TestTree
specs =
  testGroup
    "Renamer Tests"
    [ testCase "Undeclared binding is detected" testDetectUndeclaredBinding
    , testCase "Duplicate declarations in the same scope" testDetectDuplicateBindings 
    ]

testDetectUndeclaredBinding :: Assertion
testDetectUndeclaredBinding = do
  parsed <-
    assertParserRight $
      testParser
        parseStatements
        [str| 
        int main() {
          return a;
        }
        |]

  uniqueSupply <- mkUniqueSupply RenamingSection
  rename uniqueSupply parsed >>= assertRenamerLeft (BindingNotFound "a")

testDetectDuplicateBindings :: Assertion
testDetectDuplicateBindings = do
  parsed <-
    assertParserRight $
      testParser
        parseStatements
        [str| 
        int main() {
          let a = 3;
          let a = 5;
          return a;
        }
        |]

  uniqueSupply <- mkUniqueSupply RenamingSection
  rename uniqueSupply parsed >>= assertRenamerLeft (DuplicateDeclaration "a")
