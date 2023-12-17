module Day09 (solve, parse, part1, part2) where

import qualified Data.List.NonEmpty as NE

solve :: Text -> (Text, Text)
solve = (display part1 &&& display part2) . parse
  where
    display f input = input >>= f & maybe "error" show

parse :: Text -> Maybe [NonEmpty Int]
parse = traverse (nonEmpty <=< (traverse (readMaybe . toString) . words)) . lines

part1, part2 :: [NonEmpty Int] -> Maybe Int
part1 = fmap sum . traverse nextValue
part2 = fmap sum . traverse previousValue

nextValue :: NonEmpty Int -> Maybe Int
nextValue values
  | all (== 0) values = Just 0
  | otherwise =
      values & differences & nonEmpty >>= nextValue <&> (last values +)

previousValue :: NonEmpty Int -> Maybe Int
previousValue values@(h :| _)
  | all (== 0) values = Just 0
  | otherwise = values & differences & nonEmpty >>= previousValue <&> (h -)

differences :: (Num a) => NonEmpty a -> [a]
differences values = zipWith (-) (tail values) (NE.toList values)
