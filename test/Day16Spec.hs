module Day16Spec where

import Day16
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  context "examples" $ do
    it "part 1" $ part1 <$> parse exampleInput `shouldBe` Just 46
    it "part 2" $ part2 <$> parse exampleInput `shouldBe` Just 51

  context "real" $ do
    real <- realInput 16 & runIO <&> parse
    it "part 1" $ part1 <$> real `shouldBe` Just 7307
    it "part 2" $ part2 <$> real `shouldBe` Just 7635

exampleInput :: Text
exampleInput =
  ".|...\\....\n\
  \|.-.\\.....\n\
  \.....|-...\n\
  \........|.\n\
  \..........\n\
  \.........\\\n\
  \..../.\\\\..\n\
  \.-.-/..|..\n\
  \.|....-|.\\\n\
  \..//.|...."
