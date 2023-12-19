module Day13Spec where

import Day13
import Test.Hspec
import Utils

spec :: Spec
spec = do
  describe "parsing" $ do
    it "example" $ (length <$> parse exampleInput) `shouldBe` Right 2

  describe "examples" $ do
    it "part 1" $ (part1 <=< ok parse) exampleInput `shouldBe` Just 405
    it "part 2" $ (part2 <=< ok parse) exampleInput `shouldBe` Just 400

  describe "real" $ do
    real <- realInput 13 & runIO <&> ok parse
    it "part 1" $ (real >>= part1) `shouldBe` Just 29165
    it "part 2" $ (real >>= part2) `shouldBe` Just 32192

ok :: (a -> Either b c) -> a -> Maybe c
ok f = either (const empty) pure . f

exampleInput :: Text
exampleInput =
  "#.##..##.\n\
  \..#.##.#.\n\
  \##......#\n\
  \##......#\n\
  \..#.##.#.\n\
  \..##..##.\n\
  \#.#.##.#.\n\
  \\n\
  \#...##..#\n\
  \#....#..#\n\
  \..##..###\n\
  \#####.##.\n\
  \#####.##.\n\
  \..##..###\n\
  \#....#..#"
