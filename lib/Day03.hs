{-# LANGUAGE TupleSections #-}

module Day03 (solve, part1, part2, parse, Field, Part, Position) where

import Data.Array
import Data.Char (isDigit)
import Data.List (elemIndices, intersect)
import qualified Data.Text as T

type Position = (Int, Int)

type Field = Array Position Char

data Part = Part
  { row :: Int,
    columnStart :: Int,
    columnEnd :: Int,
    value :: Int
  }
  deriving (Show, Eq)

parse :: Text -> Maybe (Field, [Part], [Position])
parse input = do
  let ls = input & lines
      enumeratedLines = zip [0 ..] ls
      parts = findPotentialParts enumeratedLines
      gears = findPotentialGears enumeratedLines
  field <- viaNonEmpty createField ls
  pure (field, parts, gears)

createField :: NonEmpty Text -> Field
createField ls = listArray ((0, 0), (rowMax, colMax)) (toString $ fold ls)
  where
    rowMax = length ls - 1
    colMax = T.length (head ls) - 1

findPotentialGears :: [(Int, Text)] -> [Position]
findPotentialGears input = do
  (rowIndex, rowText) <- input
  rowText & toString & elemIndices '*' <&> (rowIndex,)

findPotentialParts :: [(Int, Text)] -> [Part]
findPotentialParts input = do
  (row_, rowString) <- input
  (colStart_, colEnd_) <- go Nothing [] (zip [0 ..] $ toString rowString)
  case rowString & toString & take (colEnd_ + 1) & drop colStart_ & readMaybe of
    Just value_ -> pure $ Part row_ colStart_ colEnd_ value_
    Nothing -> empty
  where
    go Nothing acc ((position, c) : cs)
      | isDigit c = go (Just (position, position)) acc cs
      | otherwise = go Nothing acc cs
    go (Just active@(start, _)) acc ((position, c) : cs)
      | isDigit c = go (Just (start, position)) acc cs
      | otherwise = go Nothing (active : acc) cs
    go Nothing acc [] = acc
    go (Just active) acc [] = active : acc

solve :: Text -> (Text, Text)
solve input = case parse input of
  Just parsed -> (show $ part1 parsed, show $ part2 parsed)
  _ -> ("parse error", "parse error")

part1 :: (Field, [Part], [Position]) -> Int
part1 (field, potentialParts, _) =
  potentialParts & filter (isPart field) & map value & sum

isPart :: Field -> Part -> Bool
isPart field part = any (any isSymbol . neighbours field) (partPositions part)

isSymbol :: Char -> Bool
isSymbol c = (c /= '.') && not (isDigit c)

neighbours :: Field -> Position -> [Char]
neighbours field position =
  adjacents position
    & filter (inBounds (bounds field))
    & map (field !)

adjacents :: Position -> [Position]
adjacents (r, c) =
  [ (r + 1, c - 1),
    (r + 1, c),
    (r + 1, c + 1),
    (r, c - 1),
    (r, c + 1),
    (r - 1, c - 1),
    (r - 1, c),
    (r - 1, c + 1)
  ]

inBounds :: (Position, Position) -> Position -> Bool
inBounds ((loR, loC), (hiR, hiC)) (r, c) =
  loR <= r && r <= hiR && loC <= c && c <= hiC

partPositions :: Part -> [Position]
partPositions (Part r cs ce _) = [(r, c) | c <- [cs .. ce]]

part2 :: (Field, [Part], [Position]) -> Int
part2 (_, potentialParts, potentialGears) =
  potentialGears & mapMaybe (getGear potentialParts) <&> findRatio & sum
  where
    findRatio (partA, partB) = value partA * value partB

getGear :: [Part] -> Position -> Maybe (Part, Part)
getGear parts gear = case filter (isAdjacent gear) parts of
  [partA, partB] -> Just (partA, partB)
  _ -> Nothing

isAdjacent :: Position -> Part -> Bool
isAdjacent position part =
  not $ null $ intersect (partPositions part) (adjacents position)
