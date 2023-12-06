module Day06 (solve, part1, parse) where

import Data.Char (isDigit)
import Data.Function ((&))

solve :: String -> (String, String)
solve _ = ("unimplemented", "unimplemented")

parse :: String -> Maybe [(Int, Int)]
parse input = case lines input of
  [times, distances] -> pure $ zip (toNumbers times) (toNumbers distances)
  _ -> fail "wrong number of lines"
  where
    toNumbers = map read . filter (all isDigit) . words

part1 :: [(Int, Int)] -> Int
part1 races = races & map waysToWin & product

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

{-
you want the solution of the following equations
let u = totalTime
let v = currentRecord
let t = chargeTime
f  t = t * (u - t)
     = u * t - t^2
f' t = u - 2t
maximum where f' t = 0
u - 2t = 0
t = u / 2

find t where
f t = t * (u - t) > v
to find borders, solve
(-1) * t^2 + (u) * t + (-v) = 0
t^2 + (-u) * t + (v) = 0
x = (-b +- sqrt(b^2 - 4ac)) / 2a
x = (u +- sqrt(u^2 - 4v)) / 2
-}
