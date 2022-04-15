{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import MemoryProfile
import Text.Megaparsec (Parsec, eof, many, manyTill, try)
import Text.Megaparsec.Char (asciiChar, char, digitChar, newline, space, string)

type Parser = Parsec Void Text

sectionSeparator :: Text
sectionSeparator = "-----------------------------------"

sectionHeader :: Parser Text
sectionHeader = do
  name <- manyTill asciiChar newline
  string sectionSeparator <* newline

  pure (T.pack name)

dataPoint :: Parser DataPoint
dataPoint = do
  digits <- space *> manyTill digitChar (string "  ")
  label <- manyTill asciiChar newline

  pure $ DataPoint (read digits) (T.pack label)

allocation :: Text -> Parser Allocation
allocation section_name
  | "String Report" `T.isInfixOf` section_name = string_allocs
  | otherwise = value_label
  where
    string_allocs = do
      (DataPoint total str) <- dataPoint
      allocations <- manyTill dataPoint newline

      pure $ StringAllocation str total allocations

    value_label = RegularAllocation <$> dataPoint

section :: Parser Section
section = do
  name <- sectionHeader
  data_points <- manyTill (try $ allocation name) (void newline <|> eof)

  pure (Section name data_points)

memoryProfile :: Parser MemoryProfile
memoryProfile = manyTill ((try newline *> section) <|> section) eof
