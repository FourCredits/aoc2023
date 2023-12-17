{-# LANGUAGE LambdaCase #-}

module Day12 (part1, part2, parse, solve, Input) where

import Data.Array (Array, array, bounds, listArray, (!))
import qualified Data.Text as T

data Record = Damaged | Operational | Unsure deriving (Eq, Show)

type Input = [([Record], [Int])]

solve :: Text -> (Text, Text)
solve = maybe err (show . part1 &&& show . part2) . parse
  where
    err = ("parse error", "parse error")

parse :: Text -> Maybe Input
parse = traverse parseLine . lines
  where
    parseLine input = do
      [rs, ns] <- pure $ words input
      ns' <- ns & T.splitOn "," & traverse (readMaybe . toString)
      rs' <- traverse charToRecord $ toString rs
      pure (rs', ns')
    charToRecord '.' = Just Operational
    charToRecord '?' = Just Unsure
    charToRecord '#' = Just Damaged
    charToRecord _ = Nothing

part1 :: Input -> Int
part1 = sum . fmap (uncurry arrangements)

part2 :: Input -> Int
part2 = sum . fmap (uncurry arrangements . unfoldRecords)

unfoldRecords :: ([Record], [Int]) -> ([Record], [Int])
unfoldRecords (rs, ns) =
  (intercalate [Unsure] $ replicate 5 rs, concat $ replicate 5 ns)

-- heavily inspired by this:
-- https://github.com/ileasile/aoc-23/blob/master/src/main/kotlin/day_12_2.kt
-- we compute a kind of wavefront matrix, where the element in the matrix at
-- position (i, j) has a value (a, b), where b is the number of arrangements
-- given a mask that's the mask taken from 0 to j and the record taken from 0 to
-- i, and a is the number of those arrangements that would be possible given
-- that the current value in the record were operational
arrangements :: [Record] -> [Int] -> Int
arrangements records description =
  snd $ combinations ! snd (bounds combinations)
  where
    combinations = array ((0, 0), (n, m)) assocs
    (n, m) = (length record, length mask - 1)
    record = listArray (0, length records - 1) records
    mask = buildMask description
    assocs =
      concat
        [ [((0, 0), (1, 1))],
          [((0, j), (0, 0)) | j <- [1 .. m]],
          [((i, 0), caseForEmptyMask i) | i <- [1 .. n]],
          [((i, j), computeValue i j (current i)) | i <- [1 .. n], j <- [1 .. m]]
        ]
    caseForEmptyMask i
      | couldBeOperational (current i) = combinations ! (i - 1, 0)
      | otherwise = (0, 0)
    couldBeOperational c = c == Operational || c == Unsure
    current i = record ! (i - 1)
    computeValue i j Operational = considerOperational i j
    computeValue i j Damaged = considerDamaged i j
    computeValue i j Unsure = considerOperational i j <+> considerDamaged i j
    considerOperational i j
      | mask ! j = (0, 0)
      | otherwise = let v = snd $ combinations ! (i - 1, j) in (v, v)
    considerDamaged i j
      | mask ! j = let v = fst $ combinations ! (i - 1, j - 1) in (v, v)
      | otherwise = (0, fst $ combinations ! (i, j - 1))
    (x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)

buildMask :: [Int] -> Array Int Bool
buildMask numbers =
  (numbers <> [0])
    & concatMap (\n -> False : replicate n True)
    & \nums -> listArray (0, length nums - 1) nums
