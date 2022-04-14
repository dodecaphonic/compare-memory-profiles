{-# LANGUAGE TemplateHaskell #-}

module Analysis where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import MemoryProfile (Allocation, MemoryProfile, allocationLabel, allocations, name)
import qualified MemoryProfile as MP
import Optics

type ComparisonLabel = Text

data Comparison = Comparison
  { _profileA :: Maybe Integer,
    _profileB :: Maybe Integer,
    _label :: ComparisonLabel
  }
  deriving (Eq, Show)

data ComparedSection = ComparedSection
  { _name :: Text,
    _comparison :: [Comparison]
  }
  deriving (Eq, Show)

makeLenses ''Comparison

emptyComparison :: Text -> Comparison
emptyComparison label = Comparison {_profileA = Nothing, _profileB = Nothing, _label = label}

compareProfiles :: MemoryProfile -> MemoryProfile -> [Comparison]
compareProfiles profA profB =
  Map.elems
    (Map.unionWith mergeComparisons profAComparisons profBComparisons)
  where
    mergeComparisons a b =
      emptyComparison (a ^. label)
        & profileA
        .~ (a ^. profileA)
        & profileB
        .~ (b ^. profileB)

    comparisons ::
      Lens' Comparison (Maybe Integer) ->
      Map (Text, Text) Allocation ->
      Map (Text, Text) Comparison
    comparisons env =
      fmap
        (\alloc -> emptyComparison (alloc ^. MP.allocationLabel) & env ?~ (alloc ^. MP.allocationTotal))

    profAComparisons :: Map (Text, Text) Comparison
    profAComparisons = comparisons profileA (bySectionAndLabel profA)

    profBComparisons :: Map (Text, Text) Comparison
    profBComparisons = comparisons profileB (bySectionAndLabel profB)

    bySectionAndLabel :: MemoryProfile -> Map (Text, Text) Allocation
    bySectionAndLabel =
      Map.fromList
        . toListOf
          ( traversed
              % to
                ( \section ->
                    section
                      ^.. allocations
                      % traversed
                      % to (\allocation -> ((section ^. name, allocation ^. allocationLabel), allocation))
                )
              % traversed
          )
