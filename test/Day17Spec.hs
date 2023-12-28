module Day17Spec where

import Day17
import Test.Hspec
import Utils

spec :: Spec
spec = do
  context "example" $ do
    it "part 1" $ (exampleInput & parse >>= part1) `shouldBe` Just 102

  context "real" $ do
    real <- realInput 17 & runIO <&> parse
    it "part 1" $ (real >>= part1) `shouldBe` Just 694

exampleInput :: Text
exampleInput =
  "2413432311323\n\
  \3215453535623\n\
  \3255245654254\n\
  \3446585845452\n\
  \4546657867536\n\
  \1438598798454\n\
  \4457876987766\n\
  \3637877979653\n\
  \4654967986887\n\
  \4564679986453\n\
  \1224686865563\n\
  \2546548887735\n\
  \4322674655533"
