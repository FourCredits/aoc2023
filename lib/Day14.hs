{-# LANGUAGE NamedFieldPuns #-}

module Day14 (parse, part1) where

import Data.List (partition)
import qualified Data.Set as S

type Pos = (Int, Int)

solve :: Text -> (Text, Text)
solve = (show . part1 &&& undefined) . parse

parse :: Text -> (Int, S.Set Pos, S.Set Pos)
parse input = (length $ lines input, rounded, cubic)
  where
    (rounded, cubic) = bimap toSet toSet $ partition fst rocks
    rocks = mapMaybe extractRock $ enumerateField input
    extractRock (position, 'O') = pure (True, position)
    extractRock (position, '#') = pure (False, position)
    extractRock _ = empty
    toSet = S.fromList . map snd

enumerateField :: Text -> [(Pos, Char)]
enumerateField input = do
  (rowNumber, row) <- zip [0 ..] $ lines input
  (columnNumber, cell) <- zip [0 ..] $ toString row
  pure ((rowNumber, columnNumber), cell)

part1 :: (Int, S.Set Pos, S.Set Pos) -> Int
part1 (rows, rounded, cubic) =
  power $ S.foldl' rollNorthward (start cubic rows) rounded

data Part1State = Part1State
  {unmovables :: S.Set Pos, power :: Int, numberOfRows :: Int}
  deriving (Show, Eq)

start :: S.Set Pos -> Int -> Part1State
start cubicRocks numberOfRows =
  Part1State {unmovables = cubicRocks, power = 0, numberOfRows}

rollNorthward :: Part1State -> Pos -> Part1State
rollNorthward state@(Part1State {unmovables}) pos@(r, c)
  | r' < 0 || S.member north unmovables = add pos state
  | otherwise = rollNorthward state north
  where
    north@(r', _) = (r - 1, c)

add :: Pos -> Part1State -> Part1State
add pos@(r, _) state@(Part1State {unmovables, power, numberOfRows}) =
  state {unmovables = S.insert pos unmovables, power = power + numberOfRows - r}
