{-# LANGUAGE OverloadedStrings #-}

module Main where

import Analysis (Comparison (..), compareProfiles)
import MemoryProfile (Allocation (..), DataPoint (DataPoint), Section (..))
import Parser (allocation, memoryProfile, section, sectionHeader)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Megaparsec (runParser)

main :: IO ()
main =
  defaultMain
    ( testGroup
        "all tests"
        [ parserTests,
          analysisTests
        ]
    )

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

analysisTests :: TestTree
analysisTests =
  testGroup
    "analyzing MemoryProfiles"
    [ testGroup
        "buildComparisons"
        [ testCase "merges data points from dev and prod reports" $ do
            let devProfile =
                  [ Section
                      "A Good Section"
                      [ RegularAllocation (DataPoint 141516 "/foo/bar/baz/biz.rb:114"),
                        RegularAllocation (DataPoint 1234567 "/foo/bar/baz/bosh.rb:14")
                      ]
                  ]

            let prodProfile =
                  [ Section
                      "A Good Section"
                      [ RegularAllocation (DataPoint 34567 "/foo/bar/baz/biz.rb:114"),
                        RegularAllocation (DataPoint 891011 "/foo/bar/baz/bosh.rb:14")
                      ]
                  ]

            compareProfiles devProfile prodProfile
              @?= [ Comparison
                      { _label = "/foo/bar/baz/biz.rb:114",
                        _profileA = Just 141516,
                        _profileB = Just 34567
                      },
                    Comparison
                      { _label = "/foo/bar/baz/bosh.rb:14",
                        _profileA = Just 1234567,
                        _profileB = Just 891011
                      }
                  ],
          testCase "includes data points only present on one end" $ do
            let devProfile =
                  [ Section
                      "A Good Section"
                      [ RegularAllocation (DataPoint 141516 "/foo/bar/baz/biz.rb:114")
                      ]
                  ]

            let prodProfile =
                  [ Section
                      "A Good Section"
                      [ RegularAllocation (DataPoint 891011 "/foo/bar/baz/bosh.rb:14")
                      ]
                  ]

            compareProfiles devProfile prodProfile
              @?= [ Comparison
                      { _label = "/foo/bar/baz/biz.rb:114",
                        _profileA = Just 141516,
                        _profileB = Nothing
                      },
                    Comparison
                      { _label = "/foo/bar/baz/bosh.rb:14",
                        _profileA = Nothing,
                        _profileB = Just 891011
                      }
                  ]
        ]
    ]
