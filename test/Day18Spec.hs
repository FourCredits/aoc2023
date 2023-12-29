module Day18Spec (spec) where

import Day18
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  context "example" $ do
    it "part 1" $ part1 exampleInput `shouldBe` Right 62
    it "part 2" $ part2 exampleInput `shouldBe` Right 952408144115

  context "real" $ do
    real <- realInput 18 & runIO
    it "part 1" $ part1 real `shouldBe` Right 66993
    it "part 2" $ part2 real `shouldBe` Right 177243763226648

exampleInput :: Text
exampleInput =
  "R 6 (#70c710)\n\
  \D 5 (#0dc571)\n\
  \L 2 (#5713f0)\n\
  \D 2 (#d2c081)\n\
  \R 2 (#59c680)\n\
  \D 2 (#411b91)\n\
  \L 5 (#8ceee2)\n\
  \U 2 (#caa173)\n\
  \L 1 (#1b58a2)\n\
  \U 2 (#caa171)\n\
  \R 2 (#7807d2)\n\
  \U 3 (#a77fa3)\n\
  \L 2 (#015232)\n\
  \U 2 (#7a21e3)"
