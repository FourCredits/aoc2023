module Day21Spec where

import Day21
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  -- no example for part 2, because is uses assumptions from the real input that
  -- aren't true in the example input
  context "examples" $ do
    let input = parse exampleInput
    it "part 1" $ part1 6 <$> input `shouldBe` Just 16

  context "real" $ do
    real <- realInput 21 & runIO <&> parse
    it "part 1" $ part1 64 <$> real `shouldBe` Just 3646
    it "part 2" $ part2 <$> real `shouldBe` Just 606188414811259

exampleInput :: Text
exampleInput =
  "...........\n\
  \.....###.#.\n\
  \.###.##..#.\n\
  \..#.#...#..\n\
  \....#.#....\n\
  \.##..S####.\n\
  \.##..#...#.\n\
  \.......##..\n\
  \.##.#.####.\n\
  \.##..##.##.\n\
  \..........."
