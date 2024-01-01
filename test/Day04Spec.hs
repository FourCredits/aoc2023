module Day04Spec where

import qualified Data.Set as S
import Day04 (Card (..), parse, part1, part2)
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  real <- realInput 4 & runIO

  describe "parse" $ do
    it "correctly parses the example input" $ do
      parse exampleInput `shouldBe` Right exampleParsed

  describe "part 1" $ do
    it "example input" $ do
      part1 exampleParsed `shouldBe` 13

    it "real input" $ do
      (real & parse <&> part1) `shouldBe` Right 25183

  describe "part 2" $ do
    it "example input" $ do
      part2 exampleParsed `shouldBe` 30

    it "real input" $ do
      (real & parse <&> part2) `shouldBe` Right 5667240

exampleParsed :: [Card]
exampleParsed =
  [ Card
      { winning = S.fromList [17, 41, 48, 83, 86],
        present = S.fromList [6, 9, 17, 31, 48, 53, 83, 86]
      },
    Card
      { winning = S.fromList [13, 16, 20, 32, 61],
        present = S.fromList [17, 19, 24, 30, 32, 61, 68, 82]
      },
    Card
      { winning = S.fromList [1, 21, 44, 53, 59],
        present = S.fromList [1, 14, 16, 21, 63, 69, 72, 82]
      },
    Card
      { winning = S.fromList [41, 69, 73, 84, 92],
        present = S.fromList [5, 51, 54, 58, 59, 76, 83, 84]
      },
    Card
      { winning = S.fromList [26, 28, 32, 83, 87],
        present = S.fromList [12, 22, 30, 36, 70, 82, 88, 93]
      },
    Card
      { winning = S.fromList [13, 18, 31, 56, 72],
        present = S.fromList [10, 11, 23, 35, 36, 67, 74, 77]
      }
  ]

exampleInput :: Text
exampleInput =
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
  \Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
  \Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
  \Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
  \Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
  \Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11\n"
