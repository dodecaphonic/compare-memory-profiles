{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Analysis where

import Data.Function (on)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, maybe)
import Data.Text (Text, intersperse)
import qualified Data.Text as T
import MemoryProfile (Allocation, MemoryProfile)
import qualified MemoryProfile as MP
import Optics
  ( Field1 (_1),
    Field2 (_2),
    Lens',
    filtered,
    folded,
    makeLenses,
    to,
    toListOf,
    traversed,
    view,
    (%),
    (%~),
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
  deriving (Eq)

data ComparedSection = ComparedSection
  { _name :: Text,
    _comparisons :: [Comparison]
  }
  deriving (Eq)

instance Show Comparison where
  show (Comparison profA profB label) =
    T.unpack $
      ( padLeft 13 $
          T.pack $
            maybe "             -" show profA
      )
        <> "  "
        <> ( padLeft 13 $
               T.pack $
                 maybe "             -" show profB
           )
        <> "    "
        <> label

padLeft :: Int -> Text -> Text
padLeft desiredLength = go desiredLength
  where
    go :: Int -> Text -> Text
    go n t
      | T.length t >= desiredLength = t
      | n == 0 = t
      | otherwise = go (n - 1) (" " <> t)

instance Show ComparedSection where
  show (ComparedSection name cs) =
    T.unpack name
      <> "\n-----------------------------------\n"
      <> concat ((<> "\n") . show <$> cs)

makeLenses ''Comparison
makeLenses ''ComparedSection

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

filterBy :: (Comparison -> Bool) -> [ComparedSection] -> [ComparedSection]
filterBy pred compSecs =
  compSecs
    & traversed
    % comparisons
    %~ toListOf (folded % filtered pred)

allocationDiffAbovePct :: Integer -> [ComparedSection] -> [ComparedSection]
allocationDiffAbovePct pct = filterBy (fromMaybe False . isDiffAbovePct)
  where
    isDiffAbovePct comp =
      (\a b -> (fromIntegral (abs (a - b)) / fromIntegral a) > fromIntegral pct / 100.0)
        <$> comp ^. profileA
        <*> comp ^. profileB

onlyPresentInProfileA :: [ComparedSection] -> [ComparedSection]
onlyPresentInProfileA = filterBy (isNothing . view profileB)

onlyPresentInProfileB :: [ComparedSection] -> [ComparedSection]
onlyPresentInProfileB = filterBy (isNothing . view profileA)

presentInBoth :: [ComparedSection] -> [ComparedSection]
presentInBoth = filterBy (\c -> isJust (c ^. profileA) && isJust (c ^. profileB))
