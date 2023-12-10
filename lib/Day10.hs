module Day10 (part1, solve) where

import Control.Arrow ((&&&))
import Data.Array (Array, Ix (inRange), assocs, bounds, listArray, (!))
import Data.Function ((&))
import Data.List (find)
import Data.Maybe (fromJust)

type Position = (Int, Int)

type Map = Array Position Char

solve :: String -> (String, String)
solve = (show . part1) &&& const "unimplemented"

part1 :: String -> Int
part1 input = go 1 (n1, start) (n2, start)
  where
    grid = constructMap input
    start = findStart grid
    (n1, n2) = neighboursOfStart grid start
    next pos prev = case neighbours (grid ! pos) pos of
      Right (next1, next2) -> (if next1 == prev then next2 else next1, pos)
      Left err -> error err
    go n (pos1, prev1) (pos2, prev2)
      | pos1 == pos2 = n
      | otherwise = go (n + 1) (next pos1 prev1) (next pos2 prev2)

findStart :: Map -> Position
findStart = fst . fromJust . find ((== 'S') . snd) . assocs

constructMap :: String -> Map
constructMap input = listArray ((0, 0), (r - 1, c - 1)) $ filter (/= '\n') input
  where
    r = length $ lines input
    c = length $ head $ lines input

neighboursOfStart :: Map -> Position -> (Position, Position)
neighboursOfStart grid pos =
  case allNeighbours pos & filter (\n -> inGrid n && connectsToPos n) of
    [n1, n2] -> (n1, n2)
    _ -> error "start has the wrong number of connecting pipes"
  where
    inGrid = inRange (bounds grid)
    connectsToPos neighbour = case neighbours (grid ! neighbour) neighbour of
      Right (n1, n2) -> n1 == pos || n2 == pos
      _ -> False

allNeighbours :: Position -> [Position]
allNeighbours (r, c) = [(r + 1, c), (r - 1, c), (r, c - 1), (r, c + 1)]

neighbours :: Char -> Position -> Either String (Position, Position)
neighbours '|' (r, c) = Right ((r - 1, c), (r + 1, c))
neighbours '-' (r, c) = Right ((r, c - 1), (r, c + 1))
neighbours 'L' (r, c) = Right ((r - 1, c), (r, c + 1))
neighbours 'J' (r, c) = Right ((r - 1, c), (r, c - 1))
neighbours '7' (r, c) = Right ((r + 1, c), (r, c - 1))
neighbours 'F' (r, c) = Right ((r + 1, c), (r, c + 1))
neighbours '.' pos = Left $ "unexpected ground at position " <> show pos
neighbours c pos =
  Left $ "unexpected char '" <> show c <> "' at position " <> show pos
