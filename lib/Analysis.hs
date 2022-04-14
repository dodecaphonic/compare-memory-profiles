{-# LANGUAGE TemplateHaskell #-}

module Analysis where

import Data.Function (on)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import MemoryProfile (Allocation, MemoryProfile)
import qualified MemoryProfile as MP
import Optics
  ( Field1 (_1),
    Field2 (_2),
    Lens',
    makeLenses,
    to,
    toListOf,
    traversed,
    (%),
    (&),
    (.~),
    (?~),
    (^.),
    (^..),
  )

type ComparisonLabel = Text

data Comparison = Comparison
  { _profileA :: Maybe Integer,
    _profileB :: Maybe Integer,
    _label :: ComparisonLabel
  }
  deriving (Eq, Show)

data ComparedSection = ComparedSection
  { _name :: Text,
    _comparisons :: [Comparison]
  }
  deriving (Eq, Show)

makeLenses ''Comparison

emptyComparison :: Text -> Comparison
emptyComparison label = Comparison {_profileA = Nothing, _profileB = Nothing, _label = label}

compareProfiles :: MemoryProfile -> MemoryProfile -> [ComparedSection]
compareProfiles profA profB =
  Map.unionWith mergeComparisons profAComparisons profBComparisons
    & Map.assocs
    & List.sortBy (compare `on` fst . fst)
    & List.groupBy ((==) `on` fst . fst)
    & fmap comparedSection
  where
    comparedSection :: [((Text, Text), Comparison)] -> ComparedSection
    comparedSection [] = error "Should never be empty"
    comparedSection cs@(c : _) =
      ComparedSection
        { _name = c ^. _1 % _1,
          _comparisons = cs ^.. traversed % _2
        }

    mergeComparisons :: Comparison -> Comparison -> Comparison
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
                      ^.. MP.allocations
                      % traversed
                      % to (\allocation -> ((section ^. MP.name, allocation ^. MP.allocationLabel), allocation))
                )
              % traversed
          )
