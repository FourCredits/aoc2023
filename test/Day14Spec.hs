module Day14Spec where

import Day14
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  context "examples" $ do
    it "part 1" $ part1 (parse exampleInput) `shouldBe` 136

  context "real" $ do
    real <- realInput 14 & runIO <&> parse
    it "part 1" $ part1 real `shouldBe` 105003

exampleInput :: Text
exampleInput =
  "O....#....\n\
  \O.OO#....#\n\
  \.....##...\n\
  \OO.#O....O\n\
  \.O.....O#.\n\
  \O.#..O.#.#\n\
  \..O..#O..O\n\
  \.......O..\n\
  \#....###..\n\
  \#OO..#...."
