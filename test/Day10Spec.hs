module Day10Spec where

import Data.Function ((&))
import Day10 (part1)
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  real <- realInput 10 & runIO

  describe "part 1" $ do
    it "example 1" $ do
      part1 example1 `shouldBe` 4

    it "example 2" $ do
      part1 example2 `shouldBe` 8

    it "real" $ do
      part1 real `shouldBe` 6909

example1 :: String
example1 =
  "-L|F7\n\
  \7S-7|\n\
  \L|7||\n\
  \-L-J|\n\
  \L|-JF"

example2 :: String
example2 =
  "7-F7-\n\
  \.FJ|7\n\
  \SJLL7\n\
  \|F--J\n\
  \LJ.LJ"
