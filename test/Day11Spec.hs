module Day11Spec where

import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Set as S
import Day11
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  describe "parsing" $ do
    it "example input" $ parse exampleInput `shouldBe` exampleParsed

  describe "example input" $ do
    it "returns 374 for 2 times bigger" $ part1 exampleParsed `shouldBe` 374

    it "returns 1030 for 10 times bigger" $
      distances 10 exampleParsed `shouldBe` 1030

    it "returns 8410 for 100 times bigger" $
      distances 100 exampleParsed `shouldBe` 8410

  describe "real" $ do
    real <- realInput 11 & runIO <&> parse
    it "part 1" $ part1 real `shouldBe` 9805264
    it "part 2" $ part2 real `shouldBe` 779032247216

exampleParsed :: S.Set Galaxy
exampleParsed =
  S.fromList
    [ {- 1 -} (0, 3),
      {- 2 -} (1, 7),
      {- 3 -} (2, 0),
      {- 4 -} (4, 6),
      {- 5 -} (5, 1),
      {- 6 -} (6, 9),
      {- 7 -} (8, 7),
      {- 8 -} (9, 0),
      {- 9 -} (9, 4)
    ]

exampleInput :: String
exampleInput =
  "...#......\n\
  \.......#..\n\
  \#.........\n\
  \..........\n\
  \......#...\n\
  \.#........\n\
  \.........#\n\
  \..........\n\
  \.......#..\n\
  \#...#....."
