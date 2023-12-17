{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}

module Day11 (part1, part2, parse, Galaxy, solve, distances) where

import qualified Data.Set as S

type Galaxy = (Int, Int)

solve :: Text -> (Text, Text)
solve = (show . part1 &&& show . part2) . parse

parse :: Text -> S.Set Galaxy
parse input = S.fromList $ do
  (rowIndex, rowString) <- zip [0 ..] $ lines input
  (columnIndex, character) <- zip [0 ..] $ toString rowString
  guard $ character == '#'
  pure (rowIndex, columnIndex)

part1, part2 :: S.Set Galaxy -> Int
part1 = distances 2
part2 = distances 1_000_000

distances :: Int -> S.Set Galaxy -> Int
distances expansion galaxies =
  galaxies & allPairsNoInverses & map (distance expansion rows columns) & sum
  where
    (rows, columns) = expandedSpace galaxies

allPairsNoInverses :: (Ord a) => S.Set a -> [(a, a)]
allPairsNoInverses source =
  concatMap (\g1 -> filter (uncurry (<)) $ map (g1,) $ S.toList source) source

distance :: Int -> [Int] -> [Int] -> (Galaxy, Galaxy) -> Int
distance expansion rows cols (g1@(r1, c1), g2@(r2, c2)) =
  manhattanDistance g1 g2
    + (expansion - 1) * length (slice r1 r2 rows)
    + (expansion - 1) * length (slice c1 c2 cols)

manhattanDistance :: Galaxy -> Galaxy -> Int
manhattanDistance (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2)

slice :: (Ord a) => a -> a -> [a] -> [a]
slice pos1 pos2 = takeWhile (<= end) . dropWhile (< start)
  where
    (start, end) = if pos1 < pos2 then (pos1, pos2) else (pos2, pos1)

expandedSpace :: S.Set Galaxy -> ([Int], [Int])
expandedSpace galaxies = (rows, cols)
  where
    (minR, minC, maxR, maxC) = bounds galaxies
    rows = filter (\r -> not $ any ((== r) . fst) galaxies) [minR .. maxR]
    cols = filter (\c -> not $ any ((== c) . snd) galaxies) [minC .. maxC]

bounds :: S.Set Galaxy -> (Int, Int, Int, Int)
bounds = foldl' f (maxBound, maxBound, minBound, minBound)
  where
    f (minR, minC, maxR, maxC) (r, c) =
      ( min minR r,
        min minC c,
        max maxR r,
        max maxC c
      )
