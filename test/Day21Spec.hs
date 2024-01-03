module Day21Spec where

import Day21
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  context "examples" $ do
    it "part 1" $ part1 6 <$> parse exampleInput `shouldBe` Just 16

  context "real" $ do
    real <- realInput 21 & runIO <&> parse
    it "part 1" $ part1 64 <$> real `shouldBe` Just 3646

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
