module Day07Spec where

import Data.Function ((&))
import Data.Functor ((<&>))
import Day07 (Card (..), Hand (..), parse, part1, part2)
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  real <- realInput 7 & runIO <&> parse

  describe "parse" $ do
    it "should correctly parse example input" $ do
      parse exampleInput `shouldBe` Right exampleParsed

  describe "part 1" $ do
    it "example input" $ do
      part1 exampleParsed `shouldBe` 6440

    it "real input" $ do
      part1 <$> real `shouldBe` Right 253205868

  describe "part 2" $ do
    it "example input" $ do
      part2 exampleParsed `shouldBe` 5905

    it "real input" $ do
      part2 <$> real `shouldBe` Right 253907829

exampleParsed :: [(Hand, Int)]
exampleParsed =
  [ (Hand {cards = [Three, Two, T, Three, K]}, 765),
    (Hand {cards = [T, Five, Five, J, Five]}, 684),
    (Hand {cards = [K, K, Six, Seven, Seven]}, 28),
    (Hand {cards = [K, T, J, J, T]}, 220),
    (Hand {cards = [Q, Q, Q, J, A]}, 483)
  ]

exampleInput :: Text
exampleInput =
  "32T3K 765\n\
  \T55J5 684\n\
  \KK677 28\n\
  \KTJJT 220\n\
  \QQQJA 483"
