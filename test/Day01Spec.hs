module Day01Spec where

import Data.Function ((&))
import Data.Functor ((<&>))
import Day01
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  real <- realInput 1 & runIO <&> lines

  describe "part 1" $ do
    it "returns the correct answer for the example" $ do
      part1 part1Example `shouldBe` Just 142

    it "gives the correct answer for real input" $ do
      part1 real `shouldBe` Just 53194

  describe "part 2" $ do
    it "returns the correct answer for the example" $ do
      part2 part2Example `shouldBe` Just 281

    it "gives the correct answer for real input" $ do
      part2 real `shouldBe` Just 54249

part1Example :: [String]
part1Example =
  lines
    "1abc2\n\
    \pqr3stu8vwx\n\
    \a1b2c3d4e5f\n\
    \treb7uchet"

part2Example :: [String]
part2Example =
  lines
    "two1nine\n\
    \eightwothree\n\
    \abcone2threexyz\n\
    \xtwone3four\n\
    \4nineeightseven2\n\
    \zoneight234\n\
    \7pqrstsixteen"
