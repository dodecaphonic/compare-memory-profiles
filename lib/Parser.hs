{-# LANGUAGE OverloadedStrings #-}

module Parser where

import qualified Data.String as T
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, many, manyTill)
import Text.Megaparsec.Char (asciiChar, char, digitChar, newline, space, string)

type Parser = Parsec Void Text

data DataPoint = DataPoint
  { _value :: Integer,
    _label :: Text
  }
  deriving (Eq, Show)

data Section = Section
  { _name :: Text,
    dataPoints :: [DataPoint]
  }
  deriving (Eq, Show)

type MemoryProfile = [Section]

sectionSeparator :: Text
sectionSeparator = "-----------------------------------"

sectionHeader :: Parser Text
sectionHeader = do
  name <- manyTill asciiChar newline
  string sectionSeparator <* newline

  pure (T.fromString name)

dataPoint :: Parser DataPoint
dataPoint = do
  digits <- space *> many digitChar <* string "  "
  label <- manyTill asciiChar newline

  pure $ DataPoint (read digits) (T.fromString label)

section :: Parser Section
section = do
  name <- sectionHeader
  data_points <- many dataPoint

  pure (Section name data_points)

memoryProfile :: Parser MemoryProfile
memoryProfile = many (section <* newline)
