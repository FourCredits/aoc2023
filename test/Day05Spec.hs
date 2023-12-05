module Day05Spec where

import Data.Function ((&))
import Data.Functor ((<&>))
import Day05
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  real <- realInput 5 & runIO <&> parse

  describe "parsing" $ do
    it "correctly parses input" $
      parse exampleInput `shouldBe` Right exampleParsed

  describe "part 1" $ do
    it "example input" $ part1 exampleParsed `shouldBe` 35
    it "real" $ (real <&> part1) `shouldBe` Right 525792406

  describe "part 2" $ do
    -- it "real" $ (real <&> part2) `shouldBe` Right 0
    it "example input" $ part2 exampleParsed `shouldBe` 46

exampleInput :: String
exampleInput =
  "seeds: 79 14 55 13\n\
  \\n\
  \seed-to-soil map:\n\
  \50 98 2\n\
  \52 50 48\n\
  \\n\
  \soil-to-fertilizer map:\n\
  \0 15 37\n\
  \37 52 2\n\
  \39 0 15\n\
  \\n\
  \fertilizer-to-water map:\n\
  \49 53 8\n\
  \0 11 42\n\
  \42 0 7\n\
  \57 7 4\n\
  \\n\
  \water-to-light map:\n\
  \88 18 7\n\
  \18 25 70\n\
  \\n\
  \light-to-temperature map:\n\
  \45 77 23\n\
  \81 45 19\n\
  \68 64 13\n\
  \\n\
  \temperature-to-humidity map:\n\
  \0 69 1\n\
  \1 0 69\n\
  \\n\
  \humidity-to-location map:\n\
  \60 56 37\n\
  \56 93 4"

exampleParsed :: ([Int], [Map])
exampleParsed =
  ( [79, 14, 55, 13],
    [ [ Range {destinationStart = 50, sourceStart = 98, rangeLength = 2},
        Range {destinationStart = 52, sourceStart = 50, rangeLength = 48}
      ],
      [ Range {destinationStart = 0, sourceStart = 15, rangeLength = 37},
        Range {destinationStart = 37, sourceStart = 52, rangeLength = 2},
        Range {destinationStart = 39, sourceStart = 0, rangeLength = 15}
      ],
      [ Range {destinationStart = 49, sourceStart = 53, rangeLength = 8},
        Range {destinationStart = 0, sourceStart = 11, rangeLength = 42},
        Range {destinationStart = 42, sourceStart = 0, rangeLength = 7},
        Range {destinationStart = 57, sourceStart = 7, rangeLength = 4}
      ],
      [ Range {destinationStart = 88, sourceStart = 18, rangeLength = 7},
        Range {destinationStart = 18, sourceStart = 25, rangeLength = 70}
      ],
      [ Range {destinationStart = 45, sourceStart = 77, rangeLength = 23},
        Range {destinationStart = 81, sourceStart = 45, rangeLength = 19},
        Range {destinationStart = 68, sourceStart = 64, rangeLength = 13}
      ],
      [ Range {destinationStart = 0, sourceStart = 69, rangeLength = 1},
        Range {destinationStart = 1, sourceStart = 0, rangeLength = 69}
      ],
      [ Range {destinationStart = 60, sourceStart = 56, rangeLength = 37},
        Range {destinationStart = 56, sourceStart = 93, rangeLength = 4}
      ]
    ]
  )
