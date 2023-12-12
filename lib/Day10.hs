module Day10 (part1, part2, solve, parse) where

import Control.Arrow ((&&&))
import Data.Array (Array, Ix (inRange), assocs, bounds, listArray, (!), (//))
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

-- TODO: make a type for positions, instead of using characters
parse :: String -> Input
parse input =
  let grid = constructMap input
      start = findStart grid
   in (replaceStart grid start, start)

constructMap :: String -> Map
constructMap input = listArray ((0, 0), (r - 1, c - 1)) $ filter (/= '\n') input
  where
    r = length $ lines input
    c = length $ head $ lines input

findStart :: Map -> Position
findStart = fst . fromJust . find ((== 'S') . snd) . assocs

replaceStart :: Map -> Position -> Map
replaceStart grid start@(r, c) = grid // [(start, replacement)]
  where
    replacement = case map isNeighbour allNeighbours of
      [True, True, False, False] -> '|'
      [False, False, False, False] -> '-'
      [True, False, True, False] -> '7'
      [True, False, False, True] -> 'F'
      [False, True, True, False] -> 'J'
      [False, True, False, True] -> 'L'
      _ -> error "unreachable"
    allNeighbours = [(r + 1, c), (r - 1, c), (r, c - 1), (r, c + 1)]
    isNeighbour n = inGrid n && (grid ! n) /= '.' && connectsToPos n
    inGrid = inRange (bounds grid)
    eitherIsStart (n1, n2) = n1 == start || n2 == start
    connectsToPos n = eitherIsStart $ neighbours (grid ! n) n

part1 :: Input -> Int
part1 (grid, start) = go 1 (n1, start) (n2, start)
  where
    (n1, n2) = neighbours (grid ! start) start
    next pos prev =
      let (next1, next2) = neighbours (grid ! pos) pos
       in (if next1 == prev then next2 else next1, pos)
    go n (pos1, prev1) (pos2, prev2)
      | pos1 == pos2 = n
      | otherwise = go (n + 1) (next pos1 prev1) (next pos2 prev2)

neighbours :: Char -> Position -> (Position, Position)
neighbours '|' (r, c) = ((r - 1, c), (r + 1, c))
neighbours '-' (r, c) = ((r, c - 1), (r, c + 1))
neighbours 'L' (r, c) = ((r - 1, c), (r, c + 1))
neighbours 'J' (r, c) = ((r - 1, c), (r, c - 1))
neighbours '7' (r, c) = ((r + 1, c), (r, c - 1))
neighbours 'F' (r, c) = ((r + 1, c), (r, c + 1))
neighbours '.' pos = error $ "unexpected ground at position " <> show pos
neighbours c pos =
  error $ "unexpected char '" <> show c <> "' at position " <> show pos

-- part 2

-- pick's theorem: https://en.wikipedia.org/wiki/Pick%27s_theorem
-- represented as A = i + (b / 2) - 1, where A is the area of the shape, i is
-- the number of interior points, and b is the number of points on the boundary
-- of the shape. You can rearrange this to i = A - (b / 2) + 1
part2 :: Input -> Int
part2 (grid, start) = areaOfLoop vertices - (length points `div` 2) + 1
  where
    points = getPoints grid start
    vertices = filter ((`elem` "F7JL") . (grid !)) points

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
    (_, second) = neighbours (grid ! start) start
    f (doneWithStart, current, next)
      | next == start && doneWithStart = (current, Nothing)
      | otherwise = (current, pure (True, next, next'))
      where
        next' = select $ neighbours (grid ! next) next
        select (n1, n2) = if n1 == current then n2 else n1
