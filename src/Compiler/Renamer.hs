module Compiler.Renamer where

import Compiler.Types.AST
import Compiler.Types.Name
import Compiler.Types.Unique
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Text (Text)
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader

data RenamerEnv = RenamerEnv
  { bindings :: Map PlumeName ()
  }

data RenamingError
  = BindingNotFound !Text
  | DuplicateDeclaration !Text
  deriving stock (Eq, Ord, Show)

type Renamer = Eff '[Reader UniqueSupply, Error RenamingError, IOE]

runRenamer :: UniqueSupply -> Renamer a -> IO (Either RenamingError a)
runRenamer uniqueSupply action = do
  action
    & Reader.runReader uniqueSupply
    & Error.runErrorNoCallStack
    & runEff

rename :: UniqueSupply -> AST CoreName -> IO (Either RenamingError (AST PlumeName))
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
