module Day02Spec where

import Control.Arrow (left)
import Day02
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  real <- realInput 2 & runIO <&> parseInput

  describe "part 1" $ do
    it "returns the correct answer for the example" $ do
      part1 <$> exampleInput `shouldBe` Right 8

    it "gives the correct answer for real input" $ do
      part1 <$> real `shouldBe` Right 2278

  describe "part 2" $ do
    it "returns the correct answer for the example" $ do
      part2 <$> exampleInput `shouldBe` Right 2286

    it "gives the correct answer for real input" $ do
      part2 <$> real `shouldBe` Right 67953

exampleInput :: Either Text [Game]
exampleInput =
  left show $
    parseInput
      "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
      \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
      \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
      \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
      \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
