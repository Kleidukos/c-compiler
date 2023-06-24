{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
module TestUtils where

import Compiler.Types.AST
import Utils

import GHC.Stack (HasCallStack)
import Test.Tasty.HUnit (assertFailure, (@?=))
import Compiler.Renamer

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

assertRight :: (Show a, HasCallStack) => Either a b -> IO b
assertRight (Left a) = do
  putStrLn "Test returned Left instead of Right"
  assertFailure $ "Got: " <> show a
assertRight (Right b) = pure b

assertRenamerLeft :: (Show b, HasCallStack) => RenamingError -> Either RenamingError b -> IO ()
assertRenamerLeft errA (Left errB ) | errA == errB = pure ()
                                    | otherwise =  do
  putStrLn "Renamer did not return the correct error"
  assertFailure $ "Got: " <> show errB <> " and expected: " <> show errA  
assertRenamerLeft _ (Right b) = do
  putStrLn "Renamer returned Right instead of Left"
  assertFailure $ "Got: " <> show b
