module Day18Spec where

import Day18
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  context "parsing" $ do
    it "example" $ parse exampleInput `shouldBe` Right exampleParsed

  context "example" $ do
    it "part 1" $ part1 exampleParsed `shouldBe` 62

  context "real" $ do
    real <- realInput 18 & runIO <&> parse
    it "part 1" $ part1 <$> real `shouldBe` Right 66993

exampleParsed :: [Instruction]
exampleParsed =
  [ Instruction R 6 0x70 0xC7 0x10,
    Instruction D 5 0x0D 0xC5 0x71,
    Instruction L 2 0x57 0x13 0xF0,
    Instruction D 2 0xD2 0xC0 0x81,
    Instruction R 2 0x59 0xC6 0x80,
    Instruction D 2 0x41 0x1B 0x91,
    Instruction L 5 0x8C 0xEE 0xE2,
    Instruction U 2 0xCA 0xA1 0x73,
    Instruction L 1 0x1B 0x58 0xA2,
    Instruction U 2 0xCA 0xA1 0x71,
    Instruction R 2 0x78 0x07 0xD2,
    Instruction U 3 0xA7 0x7F 0xA3,
    Instruction L 2 0x01 0x52 0x32,
    Instruction U 2 0x7A 0x21 0xE3
  ]

exampleInput :: Text
exampleInput =
  "R 6 (#70c710)\n\
  \D 5 (#0dc571)\n\
  \L 2 (#5713f0)\n\
  \D 2 (#d2c081)\n\
  \R 2 (#59c680)\n\
  \D 2 (#411b91)\n\
  \L 5 (#8ceee2)\n\
  \U 2 (#caa173)\n\
  \L 1 (#1b58a2)\n\
  \U 2 (#caa171)\n\
  \R 2 (#7807d2)\n\
  \U 3 (#a77fa3)\n\
  \L 2 (#015232)\n\
  \U 2 (#7a21e3)"
