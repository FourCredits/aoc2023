module Day08Spec where

import Data.Array (listArray)
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

    it "example 3" $ do
      parse exampleInput3 `shouldBe` Right exampleParsed3

  describe "part 1" $ do
    it "example input 1" $ do
      part1 exampleParsed1 `shouldBe` Just 2

    it "example input 2" $ do
      part1 exampleParsed2 `shouldBe` Just 6

    it "real" $ do
      part1 <$> real `shouldBe` Right (Just 11567)

  describe "part 2" $ do
    it "example input 3" $ do
      part2 exampleParsed3 `shouldBe` Just 6

    it "real" $ do
      part2 <$> real `shouldBe` Right (Just 9858474970153)

exampleParsed1, exampleParsed2, exampleParsed3 :: Input
exampleParsed1 =
  ( listArray (0, 1) [R, L],
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
  ( listArray (0, 2) [L, L, R],
    fromList
      [ ("AAA", ("BBB", "BBB")),
        ("BBB", ("AAA", "ZZZ")),
        ("ZZZ", ("ZZZ", "ZZZ"))
      ]
  )
exampleParsed3 =
  ( listArray (0, 1) [L, R],
    fromList
      [ ("11A", ("11B", "XXX")),
        ("11B", ("XXX", "11Z")),
        ("11Z", ("11B", "XXX")),
        ("22A", ("22B", "XXX")),
        ("22B", ("22C", "22C")),
        ("22C", ("22Z", "22Z")),
        ("22Z", ("22B", "22B")),
        ("XXX", ("XXX", "XXX"))
      ]
  )

exampleInput1, exampleInput2, exampleInput3 :: String
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
exampleInput3 =
  "LR\n\
  \\n\
  \11A = (11B, XXX)\n\
  \11B = (XXX, 11Z)\n\
  \11Z = (11B, XXX)\n\
  \22A = (22B, XXX)\n\
  \22B = (22C, 22C)\n\
  \22C = (22Z, 22Z)\n\
  \22Z = (22B, 22B)\n\
  \XXX = (XXX, XXX)"
