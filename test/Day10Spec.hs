module Day10Spec where

import Data.Function ((&))
import Data.Functor ((<&>))
import Day10 (parse, part1, part2)
import Test.Hspec
import Utils (realInput)

spec :: Spec
spec = do
  real <- realInput 10 & runIO <&> parse

  describe "part 1" $ do
    it "example 1" $ part1 (parse example1) `shouldBe` 4
    it "example 2" $ part1 (parse example2) `shouldBe` 8
    it "real" $ part1 real `shouldBe` 6909

  describe "part 2" $ do
    it "example 1" $ part2 (parse example1) `shouldBe` 1
    it "example 3" $ part2 (parse example3) `shouldBe` 4
    it "example 4" $ part2 (parse example4) `shouldBe` 8
    it "example 5" $ part2 (parse example5) `shouldBe` 10
    it "real" $ part2 real `shouldBe` 461

example1, example2, example3, example4, example5 :: String
example1 =
  "-L|F7\n\
  \7S-7|\n\
  \L|7||\n\
  \-L-J|\n\
  \L|-JF"
example2 =
  "7-F7-\n\
  \.FJ|7\n\
  \SJLL7\n\
  \|F--J\n\
  \LJ.LJ"
example3 =
  "...........\n\
  \.S-------7.\n\
  \.|F-----7|.\n\
  \.||.....||.\n\
  \.||.....||.\n\
  \.|L-7.F-J|.\n\
  \.|..|.|..|.\n\
  \.L--J.L--J.\n\
  \..........."
example4 =
  ".F----7F7F7F7F-7....\n\
  \.|F--7||||||||FJ....\n\
  \.||.FJ||||||||L7....\n\
  \FJL7L7LJLJ||LJ.L-7..\n\
  \L--J.L7...LJS7F-7L7.\n\
  \....F-J..F7FJ|L7L7L7\n\
  \....L7.F7||L7|.L7L7|\n\
  \.....|FJLJ|FJ|F7|.LJ\n\
  \....FJL-7.||.||||...\n\
  \....L---J.LJ.LJLJ..."
example5 =
  "FF7FSF7F7F7F7F7F---7\n\
  \L|LJ||||||||||||F--J\n\
  \FL-7LJLJ||||||LJL-77\n\
  \F--JF--7||LJLJ7F7FJ-\n\
  \L---JF-JLJ.||-FJLJJ7\n\
  \|F|F-JF---7F7-L7L|7|\n\
  \|FFJF7L7F-JF7|JL---7\n\
  \7-L-JL7||F7|L7F-7F7|\n\
  \L.L7LFJ|||||FJL7||LJ\n\
  \L7JLJL-JLJLJL--JLJ.L"
