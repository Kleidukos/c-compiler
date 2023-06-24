module Main where

import Test.Tasty

import Compiler.CodegenTest qualified as CodegenTest
import Compiler.ParserTest qualified as ParserTest
import Compiler.RenamerTest qualified as RenamerTest

--
tests :: IO TestTree
tests = do
  return $
    testGroup
      "Tests"
      [ CodegenTest.specs
      , ParserTest.specs
      , RenamerTest.specs
      ]

main :: IO ()
main = tests >>= defaultMain
