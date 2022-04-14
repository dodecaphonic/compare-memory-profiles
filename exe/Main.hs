module Main where

import Analysis (ComparedSection)
import qualified Analysis as Analysis
import Data.Either (fromRight)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import MemoryProfile (MemoryProfile)
import Parser
import Text.Megaparsec

main :: IO ()
main = do
  let path = "/home/vitorcapela/Projects/Provide/cleaner/memory_profile_deal_development.log"
  rawProfile <- T.unlines . drop 3 . T.lines <$> T.readFile path

  parseTest memoryProfile rawProfile

compare :: IO [ComparedSection]
compare = Analysis.compareProfiles <$> devLog <*> prodLog

devLog :: IO MemoryProfile
devLog = do
  let path = "/home/vitorcapela/Projects/Provide/cleaner/experiments/profiles/memory_profile_answer_50_batches_development.log"
  rawProfile <- T.unlines . drop 3 . T.lines <$> T.readFile path

  pure $ fromRight [] $ runParser memoryProfile "dev" rawProfile

prodLog :: IO MemoryProfile
prodLog = do
  let path = "/home/vitorcapela/Projects/Provide/cleaner/experiments/profiles/memory_profile_answer_50_batches_production.log"
  rawProfile <- T.unlines . drop 3 . T.lines <$> T.readFile path

  pure $ fromRight [] $ runParser memoryProfile "dev" rawProfile
