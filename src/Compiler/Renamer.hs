module Compiler.Renamer where

import Compiler.Types.AST
import Compiler.Types.Name
import Compiler.Types.Unique
import Data.Function ((&))
import Data.Map.Strict (Map)
import Effectful
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader

data RenamerEnv = RenamerEnv
  { bindings :: Map PlumeName ()
  }

type Renamer = Eff '[Reader UniqueSupply, IOE]

runRenamer :: UniqueSupply -> Renamer a -> IO a
runRenamer uniqueSupply action = do
  action
    & Reader.runReader uniqueSupply
    & runEff

rename :: UniqueSupply -> AST CoreName -> IO (AST PlumeName)
rename uniqueSupply parsedModule =
  runRenamer uniqueSupply $
    renameAST parsedModule

renameCoreName :: CoreName -> Renamer PlumeName
renameCoreName coreName = do
  uniqueSupply <- Reader.ask
  unique <- liftIO $ nextUnique uniqueSupply
  pure $ PlumeName ModuleInternal coreName unique

renameAST :: AST CoreName -> Renamer (AST PlumeName)
renameAST ast = traverse renameCoreName ast

renameExpr :: PlumeExpr CoreName -> Renamer (PlumeExpr PlumeName)
renameExpr expr = traverse renameCoreName expr
