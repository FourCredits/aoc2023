module Day09 (solve, parse, part1, part2) where

import Control.Arrow ((&&&))
import Control.Monad ((<=<))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Text.Read (readMaybe)

solve :: String -> (String, String)
solve = (display part1 &&& display part2) . parse
  where
    display f input = input >>= f & maybe "error" show

parse :: String -> Maybe [NonEmpty Int]
parse = traverse (NE.nonEmpty <=< (traverse readMaybe . words)) . lines

part1, part2 :: [NonEmpty Int] -> Maybe Int
part1 = fmap sum . traverse nextValue
part2 = fmap sum . traverse previousValue

nextValue :: NonEmpty Int -> Maybe Int
nextValue values
  | all (== 0) values = Just 0
  | otherwise =
      values & differences & NE.nonEmpty >>= nextValue <&> (NE.last values +)

previousValue :: NonEmpty Int -> Maybe Int
previousValue values@(h :| _)
  | all (== 0) values = Just 0
  | otherwise = values & differences & NE.nonEmpty >>= previousValue <&> (h -)

differences :: (Num a) => NonEmpty a -> [a]
differences values = zipWith (-) (NE.tail values) (NE.toList values)
