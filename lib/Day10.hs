module Day10 (part1, part2, solve, parse) where

import Control.Arrow ((&&&))
import Data.Array (Array, Ix (inRange), assocs, bounds, listArray, (!))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (Foldable (toList))
import Data.Function ((&))
import Data.List (find)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromJust)

type Position = (Int, Int)

type Map = Array Position Char

type Input = (Map, Position)

solve :: String -> (String, String)
solve = ((show . part1) &&& (show . part2)) . parse

-- TODO: handle start by fixing its value in the grid, seeing as we already know
-- its starting position here. that way we can simplify logic below
parse :: String -> Input
parse input = (grid, findStart grid)
  where
    grid = constructMap input

part1 :: Input -> Int
part1 (grid, start) = go 1 (n1, start) (n2, start)
  where
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

-- part 2

-- pick's theorem: https://en.wikipedia.org/wiki/Pick%27s_theorem
-- represented as A = i + (b / 2) - 1, where A is the area of the shape, i is
-- the number of interior points, and b is the number of points on the boundary
-- of the shape. You can rearrange this to i = A - (b / 2) + 1
part2 :: Input -> Int
part2 (grid, start) = areaOfLoop vertices - (length points `div` 2) + 1
  where
    points = getPoints grid start
    vertices = filter (isCorner grid) points

-- the shoelace theorem: find the area of a space by representing it as the sum
-- of signed areas of trapezoids. see here for more:
-- https://en.wikipedia.org/wiki/Shoelace_formula#Trapezoid_formula
areaOfLoop :: [Position] -> Int
areaOfLoop vertices = [1 .. len] & map trapezoid & sum & (`div` 2) & abs
  where
    trapezoid i = (r i + r (i + 1)) * (c i - c (i + 1))
    (rows, columns) = bimap toArray toArray $ unzip vertices
    r i = rows ! (i `mod` len)
    c i = columns ! (i `mod` len)
    len = length vertices
    toArray = listArray (0, len - 1)

getPoints :: Map -> Position -> [Position]
getPoints grid start = NonEmpty.unfoldr f (False, start, second) & toList
  where
    (_, second) = neighboursOfStart grid start
    f (doneWithStart, current, next)
      | next == start && doneWithStart = (current, Nothing)
      | otherwise = (current, pure (True, next, next'))
      where
        next' = either error select $ neighbours (grid ! next) next
        select (n1, n2) = if n1 == current then n2 else n1

isCorner :: Map -> Position -> Bool
isCorner grid pos =
  (grid ! pos) `elem` "F7JL" || ((grid ! pos) == 'S' && isStartCorner grid pos)

isStartCorner :: Map -> Position -> Bool
isStartCorner grid pos =
  let ((r1, c1), (r2, c2)) = neighboursOfStart grid pos
   in r1 /= r2 && c1 /= c2
