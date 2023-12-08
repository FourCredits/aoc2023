module Day08Spec where

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map (fromList)
import Day08
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  real <- realInput 8 & runIO <&> parse

  describe "parsing" $ do
    it "example 1" $ do
      parse exampleInput1 `shouldBe` Right exampleParsed1

    it "example 2" $ do
      parse exampleInput2 `shouldBe` Right exampleParsed2

  describe "part 1" $ do
    it "example input 1" $ do
      part1 exampleParsed1 `shouldBe` 2

    it "example input 2" $ do
      part1 exampleParsed2 `shouldBe` 6

    it "real" $ do
      part1 <$> real `shouldBe` Right 11567

exampleParsed1, exampleParsed2 :: Input
exampleParsed1 =
  ( [R, L],
    fromList
      [ ("AAA", ("BBB", "CCC")),
        ("BBB", ("DDD", "EEE")),
        ("CCC", ("ZZZ", "GGG")),
        ("DDD", ("DDD", "DDD")),
        ("EEE", ("EEE", "EEE")),
        ("GGG", ("GGG", "GGG")),
        ("ZZZ", ("ZZZ", "ZZZ"))
      ]
  )
exampleParsed2 =
  ( [L, L, R],
    fromList
      [ ("AAA", ("BBB", "BBB")),
        ("BBB", ("AAA", "ZZZ")),
        ("ZZZ", ("ZZZ", "ZZZ"))
      ]
  )

exampleInput1, exampleInput2 :: String
exampleInput1 =
  "RL\n\
  \\n\
  \AAA = (BBB, CCC)\n\
  \BBB = (DDD, EEE)\n\
  \CCC = (ZZZ, GGG)\n\
  \DDD = (DDD, DDD)\n\
  \EEE = (EEE, EEE)\n\
  \GGG = (GGG, GGG)\n\
  \ZZZ = (ZZZ, ZZZ)"
exampleInput2 =
  "LLR\n\
  \\n\
  \AAA = (BBB, BBB)\n\
  \BBB = (AAA, ZZZ)\n\
  \ZZZ = (ZZZ, ZZZ)"
