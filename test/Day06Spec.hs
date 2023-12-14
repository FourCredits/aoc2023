module Day06Spec where

import Day06
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  real <- runIO $ realInput 6

  describe "part 1" $ do
    it "example input" $ do
      part1 exampleInput `shouldBe` Just 288

    it "real" $ do
      part1 real `shouldBe` Just 4811940

  describe "part 2" $ do
    it "example input" $ do
      part2 exampleInput `shouldBe` Just 71503

    it "real input" $ do
      part2 real `shouldBe` Just 30077773

exampleParsed :: [(Int, Int)]
exampleParsed = [(7, 9), (15, 40), (30, 200)]

exampleInput :: Text
exampleInput =
  "Time:      7  15   30\n\
  \Distance:  9  40  200"
