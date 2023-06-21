{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
module TestUtils where

import Compiler.Types.AST
import Utils

import GHC.Stack (HasCallStack)
import Test.Tasty.HUnit (assertFailure, (@?=))

assertParserRight :: HasCallStack => Either String a -> IO a
assertParserRight (Right b) = pure b
assertParserRight (Left a) = do
  say a
  assertFailure "Test returned Left instead of Right"

assertParserLeft :: (HasCallStack, Show a) => Either b a -> IO b
assertParserLeft (Left b) = pure b
assertParserLeft (Right a) = do
  say (show a)
  assertFailure "Test returned Right instead of Left"

assertAST :: (HasCallStack, Eq name, Show name) => Either String (AST name) -> (AST name) -> IO ()
assertAST (Right result) expected = result @?= expected
assertAST (Left err) _ = do
  say err
  assertFailure "Test returned Left instead of Right"
