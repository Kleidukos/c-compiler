{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Compiler.RenamerTest where

import PyF
import Test.Tasty
import Test.Tasty.HUnit

import Compiler.Parser.Parser (parseStatements, testParser)
import Compiler.Renamer (RenamingError (..), rename)
import Compiler.Types.Unique (UniqueSection (..), mkUniqueSupply)
import Control.Monad
import TestUtils

specs :: TestTree
specs =
  testGroup
    "Renamer Tests"
    [ testCase "Undeclared binding is detected" testDetectUndeclaredBinding
    , testCase "Duplicate declarations in the same scope" testDetectDuplicateBindings
    , testCase "2-steps binding declaration" test2StepsBindingDeclaration
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
  rename uniqueSupply parsed
    >>= assertRenamerLeft (DuplicateDeclaration "a")

test2StepsBindingDeclaration :: Assertion
test2StepsBindingDeclaration = do
  parsed <-
    assertParserRight $
      testParser
        parseStatements
        [str| 
        int main() {
          int a = 6;
          return a;
        }
        |]

  uniqueSupply <- mkUniqueSupply RenamingSection
  void $
    rename uniqueSupply parsed
      >>= assertRight
