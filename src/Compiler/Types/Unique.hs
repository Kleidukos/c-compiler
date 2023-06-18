{-# LANGUAGE DataKinds #-}

module Compiler.Types.Unique where

import Prettyprinter (Doc, pretty)

data Unique = Unique !UniqueSection !Int
  deriving (Eq, Ord, Show)

-- | Provenance of Uniques.
data UniqueSection
  = -- PHC passes
    ParseSection
  | RenameSection
  | TypeCheckSection
  | DesugarSection
  | SimplifySection
  | FastStringSection
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype UniqueSupply = UniqueSupply {uniques :: [Unique]}
  deriving newtype (Eq, Show)

mkUniqueSupply :: UniqueSection -> UniqueSupply
mkUniqueSupply pass = UniqueSupply $ map (Unique pass) [0 ..]

-- nextUnique :: (State UniqueSupply :> es) => Eff es Unique
-- nextUnique = state (\(UniqueSupply s) -> (head s, UniqueSupply $ tail s))

class HasUnique a where
  getUnique :: a -> Unique

instance HasUnique Unique where
  getUnique = id

prettyUniqueSection :: UniqueSection -> Doc ann
prettyUniqueSection = \case
 ParseSection -> "p"
 RenameSection -> "rn"
 TypeCheckSection -> "tc"
 DesugarSection -> "ds"
 SimplifySection -> "simpl"
 FastStringSection -> "fs"

prettyUnique :: Unique -> Doc ann
prettyUnique (Unique pass num) = prettyUniqueSection pass <> pretty num
