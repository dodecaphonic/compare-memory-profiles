{-# LANGUAGE TemplateHaskell #-}

module Main where

import Analysis (ComparedSection)
import qualified Analysis
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import MemoryProfile (MemoryProfile)
import Optics (filtered, folded, makeLenses, view, (%), (^.), (^..))
import Options.Applicative
import Parser (memoryProfile)
import Text.Megaparsec (ParseErrorBundle, runParser)

data AppConfig = AppConfig
  { _profileAPath :: FilePath,
    _profileBPath :: FilePath,
    _profileAExclusive :: Bool,
    _profileBExclusive :: Bool,
    _allocationsDiffAbovePct :: Maybe Integer
  }

makeLenses ''AppConfig

appConfig :: Parser AppConfig
appConfig =
  AppConfig
    <$> strOption (long "profileA" <> short 'a' <> metavar "PROFILEA" <> help "Profile A (shown at the left in comparisons)")
    <*> strOption (long "profileB" <> short 'b' <> metavar "PROFILEB" <> help "Profile B (shown at the right in comparisons)")
    <*> switch (long "only-in-a" <> short 'A' <> help "Show allocations only present in profile A")
    <*> switch (long "only-in-b" <> short 'B' <> help "Show allocations only present in profile B")
    <*> optional
      ( option auto (long "allocation-diff" <> short 'D' <> metavar "PCT" <> help "Only show allocations differing PCT between A and B")
      )

main :: IO ()
main = compareProfiles =<< execParser opts
  where
    opts =
      info
        (appConfig <**> helper)
        ( fullDesc
            <> progDesc "Compare two Ruby memory_profiler dumps and apply useful filters"
            <> header "compare-memory-profiles - Compares two Ruby memory_profiler dumps"
        )

compareProfiles :: AppConfig -> IO ()
compareProfiles config = do
  profileA <- loadProfile (config ^. profileAPath)
  profileB <- loadProfile (config ^. profileBPath)

  let comparison = narrowDown <$> (Analysis.compareProfiles <$> profileA <*> profileB)

  case comparison of
    Right cs -> showComparisons cs
    Left e -> error (show e)
  where
    narrowDown :: [ComparedSection] -> [ComparedSection]
    narrowDown cs
      | config ^. profileAExclusive = Analysis.onlyPresentInProfileA cs
      | config ^. profileBExclusive = Analysis.onlyPresentInProfileB cs
      | otherwise = case config ^. allocationsDiffAbovePct of
        Just diff -> Analysis.allocationDiffAbovePct diff cs
        Nothing -> cs

showComparisons :: [ComparedSection] -> IO ()
showComparisons comparedSections = do
  let nonEmpty = comparedSections ^.. folded % filtered (not . null . view Analysis.comparisons)

  traverse_ print nonEmpty

loadProfile :: FilePath -> IO (Either (ParseErrorBundle Text Void) MemoryProfile)
loadProfile path = do
  rawProfile <- T.unlines . drop 3 . T.lines <$> T.readFile path

  pure $ runParser memoryProfile "dev" rawProfile
