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
allocationLabel = lens getLabel setLabel
  where
    getLabel = \case
      RegularAllocation dp -> dp ^. label
      StringAllocation label _ _ -> label

    setLabel alloc newLabel = case alloc of
      RegularAllocation dp -> RegularAllocation (dp & label .~ newLabel)
      StringAllocation _ bytes dps -> StringAllocation newLabel bytes dps

allocationTotal :: Lens' Allocation Integer
allocationTotal = lens getLabel setLabel
  where
    getLabel :: Allocation -> Integer
    getLabel = \case
      RegularAllocation dp -> dp ^. value
      StringAllocation _ total _ -> total

    setLabel :: Allocation -> Integer -> Allocation
    setLabel alloc newTotal = case alloc of
      RegularAllocation dp -> RegularAllocation (dp & value .~ newTotal)
      StringAllocation label newTotal dps -> StringAllocation label newTotal dps
