module Day09Spec where

import Day09
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  real <- realInput 9 & runIO <&> parse

  describe "part 1" $ do
    it "example input" $ do
      (exampleInput & parse >>= part1) `shouldBe` Just 114

    it "real" $ do
      (real >>= part1) `shouldBe` Just 1479011877

  describe "part 2" $ do
    it "example input" $ do
      (exampleInput & parse >>= part2) `shouldBe` Just 2

    it "real" $ do
      (real >>= part2) `shouldBe` Just 973

exampleInput :: Text
exampleInput =
  "0 3 6 9 12 15\n\
  \1 3 6 10 15 21\n\
  \10 13 16 21 30 45"
