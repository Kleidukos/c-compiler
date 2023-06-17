module Test (
  (==>),
  golden,
  goldenOutput,
  goldenShow,
  goldenPShow,
  CheckOutput (..),
  assertRight,
  pShowNoColorIndent2,
) where

import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Golden.Advanced
import Test.Tasty.HUnit

import Data.ByteString qualified as BS (readFile,writeFile)
import Data.ByteString.UTF8 (ByteString, fromString)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import System.FilePath

import Text.Pretty.Simple
import Data.ByteString (toStrict)

(==>) :: (Eq a, Show a) => a -> a -> Assertion
(==>) = (@?=)

class ByteStringable s where
  toByteString :: s -> ByteString

instance ByteStringable String where
  toByteString = fromString

instance ByteStringable TL.Text where
  toByteString = toStrict . TL.encodeUtf8

data CheckOutput = OK | FailFast String

-- | Test output will use @output@ from Utils.Outputable.
goldenOutput
  :: ByteStringable s
  => TestName
  -> FilePath
  -> (FilePath -> String -> a)
  -> (a -> CheckOutput)
  -> (a -> s)
  -> IO TestTree
goldenOutput name dir testee check outputFun = golden name dir testee check outputFun

{- | Test output uses pShowNoColor from pretty-simple.
use this for tests that should be human readable, which is probably all of them.
-}
goldenPShow name dir testee check = golden name dir testee check pShowNoColorIndent2

{- | Test output uses show.
Try to avoid using this one.
-}
goldenShow name dir testee check = golden name dir testee check show

pShowNoColorIndent2 :: Show a => a -> TL.Text
pShowNoColorIndent2 =
  pShowOpt
    defaultOutputOptionsDarkBg
      { outputOptionsIndentAmount = 2
      , outputOptionsColorOptions = Nothing
      }

{- | Slightly more general driver for golden tests. Also takes a function to convert
the output of the test to something that can be serialized to a golden file
(typically String or Text).
-}
golden ::
  ByteStringable s =>
  -- | name of the test tree
  TestName ->
  -- | directory to search for tests in
  FilePath ->
  -- | function to test
  (FilePath -> String -> a) ->
  -- | check if the test should fail fast
  (a -> CheckOutput) ->
  -- | function to convert output
  (a -> s) ->
  IO TestTree
golden name dir testee check display = do
  testFiles <- findByExtension [".hs"] dir
  return $
    testGroup
      name
      [ goldenTest
        (takeBaseName testFile)
        (expFrom expFile)
        (test testFile)
        (compare expFile)
        (upd expFile)
      | testFile <- testFiles
      , let expFile = replaceExtension testFile ".expected"
      ]
  where
    test :: FilePath -> IO (Either String ByteString)
    test tf = do
      inp <- readFile tf
      let r = testee tf inp
      return $ case check r of
        OK -> Right $ toByteString $ display r
        FailFast msg -> Left msg

    expFrom expFile = Right <$> BS.readFile expFile

    compare _ (Left msg) _ = pure $ Just msg
    compare _ _ (Left msg) = pure $ Just msg
    compare fn (Right bs1) (Right bs2) =
      if bs1 == bs2
        then pure Nothing
        else do
          BS.writeFile (replaceExtension fn ".actual") bs2
          return $ Just "Output was incorrect."

    upd fn (Right bs) = BS.writeFile fn bs

assertRight :: (Show a, HasCallStack) => Either a b -> IO b
assertRight (Left a) = do
  putStrLn "Test returned Left instead of Right"
  assertFailure $ "Got: " <> show a
assertRight (Right b) = pure b

-- assertRenamerError :: (Show a) => RenamerError -> Either RenamerError a -> IO ()
-- assertRenamerError expectedError (Left actualError)
--   | actualError == expectedError = pure ()
--   | otherwise = do
--       putStrLn "Test returned an unexpected Renamer error"
--       assertFailure $ "Expected: " <> show expectedError <> "\nGot: " <> show actualError
-- assertRenamerError _ (Right a) = do
--   putStrLn "Test returned Right instead of Left"
--   assertFailure $ "Got: " <> show a
