{-# LANGUAGE TupleSections #-}

module Day03 (solve, part1, part2, parse, Field, Part, Position) where

import Data.Array
import Data.Char (isDigit)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (elemIndices, intersect)
import Data.Maybe (mapMaybe)

type Position = (Int, Int)

type Field = Array Position Char

data Part = Part Int Int Int deriving (Show, Eq)

parse :: String -> (Field, [Part], [Position])
parse input =
  let ls = lines input
      enumeratedLines = zip [0 ..] ls
      numRows = length ls
      numCols = length (head ls)
      field = listArray ((0, 0), (numRows - 1, numCols - 1)) (concat ls)
      parts = findPotentialParts enumeratedLines
      gears = findPotentialGears enumeratedLines
   in (field, parts, gears)

findPotentialGears :: [(Int, String)] -> [Position]
findPotentialGears input = do
  (rowIndex, rowString) <- input
  rowString & elemIndices '*' <&> (rowIndex,)

findPotentialParts :: [(Int, String)] -> [Part]
findPotentialParts input = do
  (row_, rowString) <- input
  (colStart_, colEnd_) <- go Nothing [] (zip [0 ..] rowString)
  pure $ Part row_ colStart_ colEnd_
  where
    go Nothing acc ((position, c) : cs)
      | isDigit c = go (Just (position, position)) acc cs
      | otherwise = go Nothing acc cs
    go (Just active@(start, _)) acc ((position, c) : cs)
      | isDigit c = go (Just (start, position)) acc cs
      | otherwise = go Nothing (active : acc) cs
    go Nothing acc [] = acc
    go (Just active) acc [] = active : acc

solve :: String -> (String, String)
solve input =
  let parsed = parse input
   in (show $ part1 parsed, show $ part2 parsed)

part1 :: (Field, [Part], [Position]) -> Int
part1 (field, potentialParts, _) =
  potentialParts & filter (isPart field) & map (lookupValue field) & sum

isPart :: Field -> Part -> Bool
isPart field part = any (any isSymbol . neighbours field) (partPositions part)

isSymbol :: Char -> Bool
isSymbol c = (c /= '.') && not (isDigit c)

neighbours :: Field -> Position -> [Char]
neighbours field position = adjacents position
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
partPositions (Part r cs ce) = [(r, c) | c <- [cs .. ce]]

lookupValue :: Field -> Part -> Int
lookupValue field (Part r cs ce) = read $ [field ! (r, c) | c <- [cs .. ce]]

part2 :: (Field, [Part], [Position]) -> Int
part2 (field, potentialParts, potentialGears) =
  potentialGears & mapMaybe (getGear potentialParts) <&> findRatio & sum
  where
    findRatio (partA, partB) = lookupValue field partA * lookupValue field partB

getGear :: [Part] -> Position -> Maybe (Part, Part)
getGear parts gear = case filter (isAdjacent gear) parts of
  [partA, partB] -> Just (partA, partB)
  _ -> Nothing

isAdjacent :: Position -> Part -> Bool
isAdjacent position part =
  not $ null $ intersect (partPositions part) (adjacents position)
