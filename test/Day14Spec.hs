module Day14Spec where

import Day14
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  context "examples" $ do
    it "part 1" $ part1 <$> parse exampleInput `shouldBe` Just 136
    it "part 2" $ part2 <$> parse exampleInput `shouldBe` Just 64

  context "real" $ do
    real <- realInput 14 & runIO <&> parse
    it "part 1" $ part1 <$> real `shouldBe` Just 105003
    it "part 2" $ part2 <$> real `shouldBe` Just 93742

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
