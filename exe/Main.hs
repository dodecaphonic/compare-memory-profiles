module Main where

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

devLog :: IO MemoryProfile
devLog = do
  let path = "/home/vitorcapela/Projects/Provide/cleaner/memory_profile_deal_development.log"
  rawProfile <- T.unlines . drop 3 . T.lines <$> T.readFile path

  pure $ fromRight [] $ runParser memoryProfile "dev" rawProfile
