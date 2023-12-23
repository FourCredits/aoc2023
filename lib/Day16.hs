module Day16 (parse, part1, solve, part2) where

import Data.Array (Array, bounds, inRange, listArray, (!))
import Data.Char (isSpace)
import qualified Data.Set as S
import qualified Data.Text as T

data Tile
  = Empty
  | SplitVertical
  | SplitHorizontal
  | ForwardMirror
  | BackwardMirror

data Direction = L | R | U | D deriving (Eq, Ord, Show)

type Pos = (Int, Int)

solve :: Text -> (Text, Text)
solve = (display part1 &&& display part2) . parse
  where
    display f = maybe "parse error" (show . f)

parse :: Text -> Maybe (Array Pos Tile)
parse input = do
  let char '.' = pure Empty
      char '|' = pure SplitVertical
      char '-' = pure SplitHorizontal
      char '/' = pure ForwardMirror
      char '\\' = pure BackwardMirror
      char _ = empty
      rows = length (lines input)
  c <- viaNonEmpty (T.length . head) (lines input)
  tiles <-
    input
      & T.dropWhileEnd isSpace
      & lines
      & traverse (traverse char . toString)
  pure $ listArray ((0, 0), (rows - 1, c - 1)) (concat tiles)

part1 :: Array Pos Tile -> Int
part1 layout = energization layout (0, 0) R

energization :: Array Pos Tile -> Pos -> Direction -> Int
energization layout startPos startDirection =
  S.size $ S.map fst $ execState (go startPos startDirection) S.empty
  where
    go :: Pos -> Direction -> State (Set (Pos, Direction)) ()
    go pos direction = do
      cache <- get
      when (inRange mapBounds pos && not (S.member (pos, direction) cache)) $ do
        modify (S.insert (pos, direction))
        case layout ! pos of
          SplitVertical | direction `elem` [L, R] -> move U *> move D
          SplitHorizontal | direction `elem` [U, D] -> move L *> move R
          ForwardMirror -> move (forwardMirror direction)
          BackwardMirror -> move (backwardMirror direction)
          _ -> move direction
      where
        move dir = go (next dir pos) dir
    mapBounds = bounds layout

backwardMirror :: Direction -> Direction
backwardMirror L = U
backwardMirror R = D
backwardMirror U = L
backwardMirror D = R

forwardMirror :: Direction -> Direction
forwardMirror L = D
forwardMirror R = U
forwardMirror U = R
forwardMirror D = L

next :: Direction -> Pos -> Pos
next U (r, c) = (r - 1, c)
next D (r, c) = (r + 1, c)
next L (r, c) = (r, c - 1)
next R (r, c) = (r, c + 1)

part2 :: Array Pos Tile -> Int
part2 layout =
  let ((lowR, lowC), (highR, highC)) = bounds layout
   in foldl' max minBound $
        map (uncurry (energization layout)) $
          concat
            [ [((r, lowC), R) | r <- [lowR .. highR]],
              [((r, highC), L) | r <- [lowR .. highR]],
              [((lowR, c), D) | c <- [lowC .. highC]],
              [((highR, c), U) | c <- [lowC .. highC]]
            ]
