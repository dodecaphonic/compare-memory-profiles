{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser (DataPoint (..), Section (..), dataPoint, section, sectionHeader)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Megaparsec (runParser)

main :: IO ()
main = defaultMain parserTests

parserTests :: TestTree
parserTests =
  testGroup
    "parsing MemoryProfiler logs"
    [ testCase "parses section headers correctly" $
        runParser sectionHeader "<test>" "foo bar baz\n-----------------------------------\n"
          @?= Right "foo bar baz",
      testCase "parses a value->label dataPoint" $
        runParser dataPoint "<test>" "    141516  /foo/bar/baz/biz.rb:114\n"
          @?= Right (DataPoint 141516 "/foo/bar/baz/biz.rb:114"),
      testCase "parses a full section" $
        runParser section "<test>" "foo bar baz\n-----------------------------------\n    141516  /foo/bar/baz/biz.rb:114\n  1234567  /foo/bar/baz/bosh.rb:14\n"
          @?= Right
            ( Section
                "foo bar baz"
                [ DataPoint 141516 "/foo/bar/baz/biz.rb:114",
                  DataPoint 1234567 "/foo/bar/baz/bosh.rb:14"
                ]
            )
    ]
