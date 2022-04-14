{-# LANGUAGE OverloadedStrings #-}

module Main where

import MemoryProfile (Allocation (..), DataPoint (..), Section (..))
import Parser (allocation, memoryProfile, section, sectionHeader)
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
      testCase "parses a RegularAllocation" $
        runParser (allocation "foo bar baz") "<test>" "    141516  /foo/bar/baz/biz.rb:114\n"
          @?= Right (RegularAllocation (DataPoint 141516 "/foo/bar/baz/biz.rb:114")),
      testCase "parses a StringAllocation" $
        runParser
          (allocation "Retained String Report")
          "<test>"
          "      23  \"id\"\n      20  /foo/bar/baz/biz:33\n       3  /foo/bar/baz/bang.rb:98\n\n\n"
          @?= Right
            ( StringAllocation
                "\"id\""
                23
                [ DataPoint 20 "/foo/bar/baz/biz:33",
                  DataPoint 3 "/foo/bar/baz/bang.rb:98"
                ]
            ),
      testCase
        "parses a regular section"
        $ runParser section "<test>" "foo bar baz\n-----------------------------------\n    141516  /foo/bar/baz/biz.rb:114\n  1234567  /foo/bar/baz/bosh.rb:14\n\n"
          @?= Right
            ( Section
                "foo bar baz"
                [ RegularAllocation (DataPoint 141516 "/foo/bar/baz/biz.rb:114"),
                  RegularAllocation (DataPoint 1234567 "/foo/bar/baz/bosh.rb:14")
                ]
            ),
      testCase
        "parses a string allocations section"
        $ runParser section "<test>" "Retained String Report\n-----------------------------------\n      23  \"id\"\n      20  /foo/bar/baz/biz:33\n       3  /foo/bar/baz/bang.rb:98\n\n      23  \"bid\"\n      20  /foo/bar/baz/biz:33\n       3  /foo/bar/baz/bang.rb:98\n\n\n"
          @?= Right
            ( Section
                "Retained String Report"
                [ StringAllocation
                    "\"id\""
                    23
                    [ DataPoint 20 "/foo/bar/baz/biz:33",
                      DataPoint 3 "/foo/bar/baz/bang.rb:98"
                    ],
                  StringAllocation
                    "\"bid\""
                    23
                    [ DataPoint 20 "/foo/bar/baz/biz:33",
                      DataPoint 3 "/foo/bar/baz/bang.rb:98"
                    ]
                ]
            ),
      testCase
        "parses a string allocations section at the end of the report"
        $ runParser section "<test>" "Retained String Report\n-----------------------------------\n      23  \"id\"\n      20  /foo/bar/baz/biz:33\n       3  /foo/bar/baz/bang.rb:98\n\n\n"
          @?= Right
            ( Section
                "Retained String Report"
                [ StringAllocation
                    "\"id\""
                    23
                    [ DataPoint 20 "/foo/bar/baz/biz:33",
                      DataPoint 3 "/foo/bar/baz/bang.rb:98"
                    ]
                ]
            ),
      testCase "parses a MemoryProfile" $
        runParser
          memoryProfile
          "<test>"
          "A Good Section\n-----------------------------------\n    141516  /foo/bar/baz/biz.rb:114\n  1234567  /foo/bar/baz/bosh.rb:14\n\nAnother Good Section\n-----------------------------------\n    141516  /foo/bar/baz/biz.rb:114\n  1234567  /foo/bar/baz/bosh.rb:14\n\n\nRetained String Report\n-----------------------------------\n      23  \"id\"\n      20  /foo/bar/baz/biz:33\n       3  /foo/bar/baz/bang.rb:98\n\n      23  \"bid\"\n      20  /foo/bar/baz/biz:33\n       3  /foo/bar/baz/bang.rb:98\n\n\n"
          @?= Right
            [ Section
                "A Good Section"
                [ RegularAllocation (DataPoint 141516 "/foo/bar/baz/biz.rb:114"),
                  RegularAllocation (DataPoint 1234567 "/foo/bar/baz/bosh.rb:14")
                ],
              Section
                "Another Good Section"
                [ RegularAllocation (DataPoint 141516 "/foo/bar/baz/biz.rb:114"),
                  RegularAllocation (DataPoint 1234567 "/foo/bar/baz/bosh.rb:14")
                ],
              Section
                "Retained String Report"
                [ StringAllocation
                    "\"id\""
                    23
                    [ DataPoint 20 "/foo/bar/baz/biz:33",
                      DataPoint 3 "/foo/bar/baz/bang.rb:98"
                    ],
                  StringAllocation
                    "\"bid\""
                    23
                    [ DataPoint 20 "/foo/bar/baz/biz:33",
                      DataPoint 3 "/foo/bar/baz/bang.rb:98"
                    ]
                ]
            ]
    ]
