{-# LANGUAGE TupleSections #-}

module Day21 (parse, part1, part2, solve) where

import Data.List (maximum, (!!))
import qualified Data.Set as S

type Pos = (Int, Int)

solve :: Text -> (Text, Text)
solve = (display (part1 64) &&& display part2) . parse
  where
    display f = maybe "parse error" (show . f)

parse :: Text -> Maybe (Pos, Set Pos, Pos)
parse input = positionsOf 'S' positions & viaNonEmpty head <&> (,rocks,bounds)
  where
    rocks = S.fromList $ positionsOf '#' positions
    bounds = (maxR + 1, maxC + 1)
    maxR = maximum $ map (fst . fst) positions
    maxC = maximum $ map (snd . fst) positions
    positions = positionWise input
    positionsOf char = map fst . filter ((== char) . snd)
    positionWise board = do
      (row, line) <- zip [0 ..] $ lines board
      (column, char) <- zip [0 ..] $ toString line
      pure ((row, column), char)

part1 :: Int -> (Pos, Set Pos, Pos) -> Int
part1 steps (start, rocks, _) = S.size (iterate nextStep (one start) !! steps)
  where
    nextStep = (S.\\ rocks) . S.unions . S.map neighbours

neighbours :: Pos -> Set Pos
neighbours (r, c) = S.fromList [(r, c + 1), (r, c - 1), (r - 1, c), (r + 1, c)]

-- uses this solution as inspiration:
-- https://github.com/derailed-dash/Advent-of-Code/blob/master/src/AoC_2023/Dazbo's_Advent_of_Code_2023.ipynb
-- in short, the area that you can reach when there are no rocks is a diamond.
-- so it's area would just be x * x, where x is the number of steps you take,
-- divided by 2 (basically because of how a manhattan grid works).
-- however, the rocks complicate this, by providing obstacles. that said,
-- the rocks can't introduce high-order elements complicating the area, so we
-- know it's a quadratic function. all we need to do, then, is determine the
-- coefficients of that function, which we do by finding three points on that
-- curve.
part2 :: (Pos, Set Pos, Pos) -> Int
part2 (start, rocks, (maxR, maxC)) = a * (x * x) + b * x + c
  where
    points = map (S.size . snd3) $ iterate nextStep (mempty, mempty, one start)
    (p, q, r) = (points !! 65, points !! (65 + 131), points !! (65 + 131 + 131))
    a = q - p - b
    b = (4 * q - 3 * p - r) `div` 2
    c = p
    s = 26501365
    x = (s - 65) `div` 131
    snd3 (_, y, _) = y
    nextStep (prev, current, frontier) = (current, current', frontier')
      where
        frontier' = frontier & S.map neighbours & S.unions & filterOut
        filterOut = (S.\\ prev) . S.filter isNotRock
        current' = S.union prev frontier'
    isNotRock (row, col) = (row `mod` maxR, col `mod` maxC) `S.notMember` rocks
