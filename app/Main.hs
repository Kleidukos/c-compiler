module Main where

import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Text.IO qualified as Text
import System.Directory qualified as Directory

import Compiler.Codegen.X86_64 (runCodeGen)
import Compiler.Parser.Parser qualified as Parser
import Compiler.Renamer qualified as Renamer
import Compiler.Types.Unique
import System.Exit (die)
import System.Process.Typed (readProcessStdout_)

main :: IO ()
main = putStrLn "lol"

compileModule
  :: FilePath
  -- ^ Source name
  -> String
  -- ^ Source
  -> IO ()
compileModule sourceName source = do
  let parseResult = Parser.parse Parser.parseStatements sourceName source
  case parseResult of
    Left err -> die (show err)
    Right parsed -> do
      uniqueSupply <- mkUniqueSupply RenamingSection
      renameResult <- Right <$> Renamer.rename uniqueSupply parsed
      case renameResult of
        Left err -> die (show err)
        Right renamed -> do
          asm <- runCodeGen renamed
          targetTriplet <- readProcessStdout_ "gcc -dumpmachine"
          let buildDir = "./build/" <> LBS.unpack targetTriplet
          Directory.createDirectoryIfMissing True buildDir
          Text.writeFile (buildDir <> "/" <> sourceName <> ".s") asm
          putStrLn $ "Finished compiling: " <> sourceName
