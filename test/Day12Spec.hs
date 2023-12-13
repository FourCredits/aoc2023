module Day12Spec where

import Data.Function ((&))
import Day12
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  real <- realInput 12 & runIO

  describe "part 1" $ do
    it "example input" $ part1 exampleInput `shouldBe` Just 21

    it "real" $ part1 real `shouldBe` Just 6935

exampleInput :: String
exampleInput =
  "???.### 1,1,3\n\
  \.??..??...?##. 1,1,3\n\
  \?#?#?#?#?#?#?#? 1,3,1,6\n\
  \????.#...#... 4,1,1\n\
  \????.######..#####. 1,6,5\n\
  \?###???????? 3,2,1"
