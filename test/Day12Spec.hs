module Day12Spec where

import Day12
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  describe "example taken from ilya's code" $ do
    it "part 1" $ part1 <$> parse ilyaExample `shouldBe` Just 6

  describe "examples" $ do
    it "part 1" $ part1 <$> exampleParsed `shouldBe` Just 21
    it "part 2" $ part2 <$> exampleParsed `shouldBe` Just 525152

  describe "real" $ do
    real <- realInput 12 & runIO <&> parse
    it "part 1" $ part1 <$> real `shouldBe` Just 6935
    it "part 2" $ part2 <$> real `shouldBe` Just 3920437278260

exampleParsed :: Maybe Input
exampleParsed = parse exampleInput

ilyaExample :: Text
ilyaExample = "????.??#.?? 1,1,1,1,1"

exampleInput :: Text
exampleInput =
  "???.### 1,1,3\n\
  \.??..??...?##. 1,1,3\n\
  \?#?#?#?#?#?#?#? 1,3,1,6\n\
  \????.#...#... 4,1,1\n\
  \????.######..#####. 1,6,5\n\
  \?###???????? 3,2,1"
