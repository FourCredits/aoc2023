module Day03Spec where

import Data.Function ((&))
import Day03
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  real <- realInput 3 & runIO

  describe "part 1" $ do
    it "example input" $ do
      part1 exampleInput `shouldBe` 4361

    it "real value" $ do
      part1 real `shouldBe` 0

exampleInput :: String
exampleInput =
  "467..114..\n\
  \...*......\n\
  \..35..633.\n\
  \......#...\n\
  \617*......\n\
  \.....+.58.\n\
  \..592.....\n\
  \......755.\n\
  \...$.*....\n\
  \.664.598.."
