module Compiler.Renamer where

import Compiler.Types.AST
import Compiler.Types.Name
import Compiler.Types.Unique
import Data.Function ((&))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as Vector
import Effectful
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static qualified as Reader
import Effectful.State.Static.Local (State)
import Effectful.State.Static.Local qualified as State

data RenamerEnv = RenamerEnv
  { bindings :: Set CoreName
  }

emptyRenamerEnv :: RenamerEnv
emptyRenamerEnv = RenamerEnv Set.empty

data RenamingError
  = BindingNotFound !Text
  | DuplicateDeclaration !Text
  deriving stock (Eq, Ord, Show)

type Renamer = Eff '[Reader UniqueSupply, State RenamerEnv, Error RenamingError, IOE]

runRenamer :: UniqueSupply -> Renamer a -> IO (Either RenamingError a)
runRenamer uniqueSupply action = do
  action
    & Reader.runReader uniqueSupply
    & State.evalState emptyRenamerEnv
    & Error.runErrorNoCallStack
    & runEff

rename :: UniqueSupply -> AST CoreName -> IO (Either RenamingError (AST PlumeName))
rename uniqueSupply parsedModule =
  runRenamer uniqueSupply $
    renameAST parsedModule

-- | Transform a CoreName to a PlumeName and add the CoreName to the renamer environment
renameCoreName :: CoreName -> Renamer PlumeName
renameCoreName coreName = do
  uniqueSupply <- Reader.ask
  unique <- liftIO $ nextUnique uniqueSupply
  let renamedName = PlumeName ModuleInternal coreName unique
  addName coreName 
  pure renamedName

renameAST :: AST CoreName -> Renamer (AST PlumeName)
renameAST ast = case ast of
  Let name body -> do
    renamedName <- guardDuplicate name
    renamedBody <- renameExpr body
    pure $ Let renamedName renamedBody
  Fun returnType functionName binders body -> do
    renamedReturnType <- traverse renameCoreName returnType
    renamedFunctionName <- guardDuplicate functionName
    addName functionName 
    renamedBinders <- mapM (\(PatternVar name) -> PatternVar <$> renameCoreName name) binders
    renamedBody <- renameAST body
    pure $ Fun renamedReturnType renamedFunctionName renamedBinders renamedBody
  Return expr -> Return <$> renameExpr expr
  Block asts -> Block <$> Vector.mapM renameAST asts

renameExpr :: PlumeExpr CoreName -> Renamer (PlumeExpr PlumeName)
renameExpr (Var name) = Var <$> guardNotFound name
renameExpr expr = traverse renameCoreName expr

guardNotFound :: CoreName -> Renamer PlumeName 
guardNotFound name = do
    RenamerEnv{bindings} <- State.get
    if Set.member name bindings
    then renameCoreName name
    else Error.throwError $ BindingNotFound name.nameText

guardDuplicate :: CoreName -> Renamer PlumeName
guardDuplicate name = do
    RenamerEnv{bindings} <- State.get
    if Set.member name bindings
    then Error.throwError $ DuplicateDeclaration name.nameText
    else renameCoreName name

addName :: CoreName -> Renamer ()
addName name = State.modify (\env -> RenamerEnv (Set.insert name env.bindings))
