{-# LANGUAGE ViewPatterns #-}

module Day17 (parse, part1, part2) where

import Data.Array (Array, bounds, inRange, listArray, (!))
import Data.Char (isDigit)
import qualified Data.Map as M
import qualified Data.Text as T

type Pos = (Int, Int)

data Direction = L | R | U | D deriving (Eq, Ord, Show)

data AStarState = AStarState
  { position :: Pos,
    heatLoss :: Int,
    direction :: Direction,
    straightLine :: Int
  }
  deriving (Eq, Show)

instance Ord AStarState where
  compare = compare `on` heatLoss

parse :: Text -> Maybe (Array Pos Int)
parse input = do
  let rows = length $ lines input
      toNum c = if inRange ('0', '9') c then pure $ ord c - ord '0' else Nothing
  columns <- viaNonEmpty (T.length . head) (lines input)
  numbers <- traverse toNum $ filter isDigit $ toString input
  pure $ listArray ((0, 0), (rows - 1, columns - 1)) numbers

part1 :: Array Pos Int -> Maybe Int
part1 cityMap = go (insert start M.empty) M.empty
  where
    go (M.minView -> Just (current@(AStarState p hL _ _), rest)) visited
      | p == end = pure hL
      | isJust $ mfilter (<= hL) $ visited M.!? toKey current = go rest visited
      | otherwise =
          let priorityQueue' = foldr insert rest (options current)
           in go priorityQueue' (M.insert (toKey current) hL visited)
    go _ _ = empty
    toKey (AStarState p _ d sl) = (p, d, sl)
    insert = liftA2 M.insert (\(AStarState p hl d sl) -> (hl, p, d, sl)) id
    start =
      AStarState
        { position = startPos,
          heatLoss = 0,
          direction = D,
          straightLine = 0
        }
    cityLimits@(startPos, end) = bounds cityMap
    options (AStarState pos loss dir straight) =
      filter (inRange cityLimits . position) $ case straight of
        3 -> [left, right]
        _ -> [left, forward, right]
      where
        forward = newState dir straight
        newState d sl =
          let pos' = next pos d
           in AStarState pos' (loss + (cityMap ! pos')) d (sl + 1)
        (left, right) = case dir of
          L -> (newState D 0, newState U 0)
          R -> (newState U 0, newState D 0)
          U -> (newState L 0, newState R 0)
          D -> (newState R 0, newState L 0)

part2 :: Array Pos Int -> Maybe Int
part2 cityMap =
  go (foldr insert M.empty [start, start {direction = R}]) M.empty
  where
    go (M.minView -> Just (current@(AStarState p hL _ sL), rest)) visited
      | p == end && sL >= 4 = pure hL
      | isJust $ mfilter (<= hL) $ visited M.!? toKey current = go rest visited
      | otherwise =
          let priorityQueue' = foldr insert rest (options current)
           in go priorityQueue' (M.insert (toKey current) hL visited)
    go _ _ = empty
    toKey (AStarState p _ d sl) = (p, d, sl)
    insert = liftA2 M.insert (\(AStarState p hl d sl) -> (hl, p, d, sl)) id
    start =
      AStarState
        { position = startPos,
          heatLoss = 0,
          direction = D,
          straightLine = 0
        }
    cityLimits@(startPos, end) = bounds cityMap
    options (AStarState pos loss dir straight) =
      filter (inRange cityLimits . position) $ case straight of
        n
          | n < 4 -> [forward]
          | n >= 10 -> [left, right]
          | otherwise -> [left, right, forward]
      where
        forward = newState dir straight
        newState d sl =
          let pos' = next pos d
           in AStarState pos' (loss + (cityMap ! pos')) d (sl + 1)
        (left, right) = case dir of
          L -> (newState D 0, newState U 0)
          R -> (newState U 0, newState D 0)
          U -> (newState L 0, newState R 0)
          D -> (newState R 0, newState L 0)

next :: Pos -> Direction -> Pos
next (r, c) L = (r, c - 1)
next (r, c) R = (r, c + 1)
next (r, c) U = (r - 1, c)
next (r, c) D = (r + 1, c)
