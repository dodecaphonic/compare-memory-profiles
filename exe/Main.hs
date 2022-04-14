module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Parser
import Text.Megaparsec

main :: IO ()
main = do
  let path = "/home/vitorcapela/Projects/Provide/cleaner/memory_profile_deal_development.log"
  rawProfile <- T.unlines . drop 3 . T.lines <$> T.readFile path

  parseTest memoryProfile rawProfile
