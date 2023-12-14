module Day06 (solve, part1, part2) where

import Data.Char (isDigit)
import qualified Data.Text as T

solve :: Text -> (Text, Text)
solve = (maybe "error" show . part1) &&& (maybe "error" show . part2)

parse1 :: Text -> Maybe [(Int, Int)]
parse1 input = case input & lines <&> toNumbers of
  [times, distances] -> pure $ zip times distances
  _ -> empty
  where
    toNumbers = mapMaybe (readMaybe . toString) . filter (T.all isDigit) . words

parse2 :: Text -> Maybe (Int, Int)
parse2 input = case input & lines & mapMaybe toNumbers of
  [time, distance] -> pure (time, distance)
  _ -> empty
  where
    toNumbers = readMaybe . filter isDigit . toString

part1, part2 :: Text -> Maybe Int
part1 input = (input & parse1) <&> product . map waysToWin
part2 input = input & parse2 <&> waysToWin

waysToWin :: (Int, Int) -> Int
waysToWin (totalTime, record) =
  totalTime
    & possibleDistances
    & filter (> record)
    & length

possibleDistances :: Int -> [Int]
possibleDistances totalTime =
  map (distanceTravelled totalTime) [0 .. totalTime]

distanceTravelled :: Int -> Int -> Int
distanceTravelled totalTime chargeTime = chargeTime * (totalTime - chargeTime)
