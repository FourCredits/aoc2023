module Day15Spec where

import Day15
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  context "examples" $ do
    it "part 1" $ part1 exampleInput `shouldBe` 1320
    it "part 2" $ part2 exampleInput `shouldBe` Just 145

  context "real" $ do
    real <- realInput 15 & runIO
    it "part 1" $ part1 real `shouldBe` 502139
    it "part 2" $ part2 real `shouldBe` Just 284132

exampleInput :: Text
exampleInput = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
