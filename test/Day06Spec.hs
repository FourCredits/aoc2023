module Day06Spec where

import Data.Function ((&))
import Data.Functor ((<&>))
import Day06
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  real <- realInput 6 & runIO <&> parse

  describe "part 1" $ do
    it "example input" $ do
      part1 exampleParsed `shouldBe` 288

    it "real" $ do
      part1 <$> real `shouldBe` Just 4811940

exampleParsed :: [(Int, Int)]
exampleParsed = [(7, 9), (15, 40), (30, 200)]

exampleInput :: String
exampleInput =
  "Time:      7  15   30\n\
  \Distance:  9  40  200"
