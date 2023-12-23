module Day16Spec where

import Day16
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  context "examples" $ do
    it "part 1" $ part1 <$> parse exampleInput `shouldBe` Just 46

  context "real" $ do
    real <- realInput 16 & runIO <&> parse
    it "part 1" $ part1 <$> real `shouldBe` Just 7307

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
