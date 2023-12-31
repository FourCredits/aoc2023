module Day03Spec where

import Day03
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  real <- realInput 3 & runIO <&> parse

  describe "part 1" $ do
    it "example input" $ do
      part1 <$> exampleInput `shouldBe` Just 4361

    it "real value" $ do
      part1 <$> real `shouldBe` Just 521515

  describe "part 2" $ do
    it "example input" $ do
      part2 <$> exampleInput `shouldBe` Just 467835

    it "real value" $ do
      part2 <$> real `shouldBe` Just 69527306

exampleInput :: Maybe (Field, [Part], [Position])
exampleInput =
  parse
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
