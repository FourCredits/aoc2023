{-# LANGUAGE NumericUnderscores #-}

module Day20Spec (spec) where

import Day20
import Test.Hspec
import Utils

spec :: Spec
spec = do
  context "examples" $ do
    it "part 1, example 1" $
      part1 <$> parse exampleInput1 `shouldBe` Right 32_000_000
    it "part 1, example 2" $
      part1 <$> parse exampleInput2 `shouldBe` Right 11_687_500

  context "real" $ do
    real <- realInput 20 & runIO <&> parse
    it "part1" $ part1 <$> real `shouldBe` Right 861_743_850

exampleInput1, exampleInput2 :: Text
exampleInput1 =
  "broadcaster -> a, b, c\n\
  \%a -> b\n\
  \%b -> c\n\
  \%c -> inv\n\
  \&inv -> a"
exampleInput2 =
  "broadcaster -> a\n\
  \%a -> inv, con\n\
  \&inv -> b\n\
  \%b -> con\n\
  \&con -> output"
