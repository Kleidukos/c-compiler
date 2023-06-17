module Main where

import Test.Tasty

import Compiler.CodegenTest qualified as CodegenTest
import Compiler.ParserTest qualified as ParserTest

--
tests :: IO TestTree
tests = do
  return $
    testGroup
      "Tests"
      [ CodegenTest.specs
      , ParserTest.specs
      ]

main :: IO ()
main = tests >>= defaultMain
