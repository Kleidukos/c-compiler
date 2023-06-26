{-# LANGUAGE DataKinds #-}

module Compiler.Types.Unique where

import Control.Concurrent.Counter (Counter)
import Control.Concurrent.Counter qualified as Counter
import Prettyprinter (Doc, pretty)

data Unique = Unique !UniqueSection !Int
  deriving (Eq, Ord, Show)

-- | Provenance of Uniques.
data UniqueSection
  = -- Compiler passes
    ParseSection
  | RenamingSection
  | TypeCheckSection
  | CodeGenSection
  deriving (Eq, Ord, Show, Enum, Bounded)

data UniqueSupply = UniqueSupply
  { section :: !UniqueSection
  , counter :: !Counter
  }
  deriving stock (Eq)

mkUniqueSupply :: UniqueSection -> IO UniqueSupply
mkUniqueSupply section = do
  counter <- Counter.new 0
  pure $ UniqueSupply section counter

nextUnique :: UniqueSupply -> IO Unique
nextUnique (UniqueSupply section counter) = do
  newUniqueInt <- Counter.get counter
  Counter.set counter (newUniqueInt + 1)
  pure $ Unique section newUniqueInt

prettyUniqueSection :: UniqueSection -> Doc ann
prettyUniqueSection = \case
  ParseSection -> "p"
  RenamingSection -> "rn"
  TypeCheckSection -> "tc"
  CodeGenSection -> "cg"

prettyUnique :: Unique -> Doc ann
prettyUnique (Unique pass num) = "_" <> prettyUniqueSection pass <> pretty num
