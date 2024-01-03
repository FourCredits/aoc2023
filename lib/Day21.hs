module Day21 (parse, part1) where

import Data.List ((!!))
import qualified Data.Set as S

type Pos = (Int, Int)

parse :: Text -> Maybe (Pos, Set Pos)
parse input = case (positionsOf 'S' &&& positionsOf '#') $ positionWise input of
  ([startPos], rocks) -> Just (startPos, S.fromList rocks)
  _ -> Nothing
  where
    positionsOf char positions = map fst $ filter ((== char) . snd) positions
    positionWise board = do
      (row, line) <- zip [0 ..] $ lines board
      (column, char) <- zip [0 ..] $ toString line
      pure ((row, column), char)

part1 :: Int -> (Pos, Set Pos) -> Int
part1 steps (start, rocks) = S.size (iterate nextStep (one start) !! steps)
  where
    nextStep acc = (`S.difference` rocks) $ S.unions $ S.map neighbours acc

neighbours :: Pos -> Set Pos
neighbours (r, c) = S.fromList [(r, c + 1), (r, c - 1), (r - 1, c), (r + 1, c)]
