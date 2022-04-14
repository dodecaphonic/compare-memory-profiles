{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module MemoryProfile where

import Data.Text (Text)
import Optics

data DataPoint = DataPoint
  { _value :: Integer,
    _label :: Text
  }
  deriving (Eq, Show)

data Allocation
  = RegularAllocation DataPoint
  | StringAllocation Text Integer [DataPoint]
  deriving (Eq, Show)

data Section = Section
  { _name :: Text,
    _allocations :: [Allocation]
  }
  deriving (Eq, Show)

type MemoryProfile = [Section]

makeLenses ''Section
makeLenses ''DataPoint

allocationLabel :: Lens' Allocation Text
allocationLabel = lens getLabel undefined
  where
    getLabel = \case
      RegularAllocation dp -> dp ^. label
      StringAllocation label _ _ -> label

    setLabel newLabel = \case
      RegularAllocation dp -> RegularAllocation (dp & label .~ newLabel)
      StringAllocation _ bytes dps -> StringAllocation newLabel bytes dps

allocationTotal :: Lens' Allocation Integer
allocationTotal = lens getLabel undefined
  where
    getLabel = \case
      RegularAllocation dp -> dp ^. value
      StringAllocation _ total _ -> total

    setLabel newTotal = \case
      RegularAllocation dp -> RegularAllocation (dp & value .~ newTotal)
      StringAllocation label newTotal dps -> StringAllocation label newTotal dps
