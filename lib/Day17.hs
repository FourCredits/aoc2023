{-# LANGUAGE ViewPatterns #-}

module Day17 (parse, part1, part2, solve) where

import Data.Array (Array, bounds, inRange, listArray, (!))
import Data.Char (isDigit)
import qualified Data.Map as M
import qualified Data.Text as T

type Pos = (Int, Int)

data Direction = L | R | U | D deriving (Eq, Ord, Show)

data DayState = CrucibleState
  { position :: Pos,
    heatLoss :: Int,
    direction :: Direction,
    straightLine :: Int
  }
  deriving (Eq, Show)

instance Ord DayState where
  compare = compare `on` heatLoss

type TurningRules = Int -> (DayState, DayState, DayState) -> [DayState]

parse :: Text -> Maybe (Array Pos Int)
parse input = do
  let rows = length $ lines input
      toNum c = if inRange ('0', '9') c then pure $ ord c - ord '0' else Nothing
  columns <- viaNonEmpty (T.length . head) (lines input)
  numbers <- traverse toNum $ filter isDigit $ toString input
  pure $ listArray ((0, 0), (rows - 1, columns - 1)) numbers

solve :: Text -> (Text, Text)
solve = (display part1 &&& display part2) . parse
  where
    display f = maybe "parse error" (maybe "program error" show . f)

part1 :: Array Pos Int -> Maybe Int
part1 cityMap = aStar isFinished newOptions (mkStart cityMap)
  where
    isFinished (CrucibleState p _ _ _) = p == end
    (_, end) = bounds cityMap
    crucibleTurningRules n (left, forward, right)
      | n >= 3 = [left, right]
      | otherwise = [left, forward, right]
    newOptions = mkNewOptions cityMap crucibleTurningRules

part2 :: Array Pos Int -> Maybe Int
part2 cityMap = aStar isFinished newOptions (mkStart cityMap)
  where
    isFinished (CrucibleState p _ _ sL) = p == end && sL >= 4
    (_, end) = bounds cityMap
    newOptions = mkNewOptions cityMap ultraCrucibleTurningRules
    ultraCrucibleTurningRules n (left, forward, right)
      | n < 4 = [forward]
      | n >= 10 = [left, right]
      | otherwise = [left, forward, right]

aStar :: (DayState -> Bool) -> (DayState -> [DayState]) -> [DayState] -> Maybe Int
aStar isFinished newOptions starts = go (foldr insert M.empty starts) M.empty
  where
    go (M.minView -> Just (current@(CrucibleState _ hL _ _), rest)) visited
      | isFinished current = pure hL
      | isJust $ mfilter (<= hL) $ visited M.!? toKey current = go rest visited
      | otherwise =
          let priorityQueue' = foldr insert rest (newOptions current)
           in go priorityQueue' (M.insert (toKey current) hL visited)
    go _ _ = empty
    toKey (CrucibleState p _ d sl) = (p, d, sl)
    insert = liftA2 M.insert (\(CrucibleState p hl d sl) -> (hl, p, d, sl)) id

mkStart :: Array Pos Int -> [DayState]
mkStart cityMap = [down, down {direction = R}]
  where
    down =
      CrucibleState
        { position = start,
          heatLoss = 0,
          direction = D,
          straightLine = 0
        }
    (start, _) = bounds cityMap

mkNewOptions :: Array Pos Int -> TurningRules -> DayState -> [DayState]
mkNewOptions cityMap f (CrucibleState pos loss dir straight) =
  filter (inRange cityLimits . position) $ f straight (left, forward, right)
  where
    cityLimits = bounds cityMap
    forward = newState dir straight
    newState d sl =
      let pos' = next pos d
       in CrucibleState pos' (loss + (cityMap ! pos')) d (sl + 1)
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
