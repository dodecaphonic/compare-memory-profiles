{-# LANGUAGE TemplateHaskell #-}

module MemoryProfile where

import Data.Text (Text)
import Optics.TH

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
