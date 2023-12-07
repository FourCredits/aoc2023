module Day06 (solve, part1, part2) where

import Control.Arrow ((&&&))
import Data.Char (isDigit)
import Data.Function ((&))
import Data.Functor ((<&>))

solve :: String -> (String, String)
solve = (maybe "error" show . part1) &&& (maybe "error" show . part2)

parse1 :: String -> Maybe [(Int, Int)]
parse1 input = case input & lines <&> toNumbers of
  [times, distances] -> pure $ zip times distances
  _ -> fail "wrong number of lines"
  where
    toNumbers = map read . filter (all isDigit) . words

parse2 :: String -> Maybe (Int, Int)
parse2 input = case input & lines <&> read . filter isDigit of
  [time, distance] -> pure (time, distance)
  _ -> fail "wront number of lines"

part1, part2 :: String -> Maybe Int
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
