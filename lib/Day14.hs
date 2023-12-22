{-# LANGUAGE NamedFieldPuns #-}

module Day14 (parse, part1, solve, part2) where

import Data.List (partition)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

type Pos = (Int, Int)

data RockState = RockState
  { cubicRocks :: S.Set Pos,
    settled :: S.Set Pos,
    numberOfRows :: Int,
    numberOfColumns :: Int
  }
  deriving (Show, Eq)

solve :: Text -> (Text, Text)
solve = (display part1 &&& display part2) . parse
  where
    display f = maybe "parse error" (show . f)

parse :: Text -> Maybe RockState
parse input =
  columns <&> \c ->
    RockState
      { cubicRocks = cubic,
        settled = rounded,
        numberOfRows = length (lines input),
        numberOfColumns = c
      }
  where
    columns = viaNonEmpty (T.length . head) (lines input)
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

part1 :: RockState -> Int
part1 = power . tilt north

tilt :: (Ord a) => (Pos -> Pos, Pos -> a) -> RockState -> RockState
tilt (direction, ordering) currentState@(RockState {settled}) =
  settled
    & toList
    & sortWith ordering
    & foldl' (roll direction) (currentState {settled = S.empty})

roll :: (Pos -> Pos) -> RockState -> Pos -> RockState
roll direction currentState@(RockState {cubicRocks, settled}) pos
  | outOfBounds currentState next || alreadyTaken =
      currentState {settled = S.insert pos settled}
  | otherwise = roll direction currentState next
  where
    alreadyTaken = S.member next cubicRocks || S.member next settled
    next = direction pos

outOfBounds :: RockState -> Pos -> Bool
outOfBounds (RockState {numberOfRows, numberOfColumns}) (r, c) =
  r < 0 || r >= numberOfRows || c < 0 || c >= numberOfColumns

north, west :: (Pos -> Pos, Pos -> Int)
north = (\(r, c) -> (r - 1, c), fst)
west = (\(r, c) -> (r, c - 1), snd)

south, east :: (Pos -> Pos, Pos -> Reverse Int)
south = (\(r, c) -> (r + 1, c), Reverse . fst)
east = (\(r, c) -> (r, c + 1), Reverse . snd)

newtype Reverse a = Reverse {getReverse :: a} deriving (Eq, Show)

instance (Ord a) => Ord (Reverse a) where
  (Reverse a) <= (Reverse b) = b <= a

power :: RockState -> Int
power (RockState {settled, numberOfRows}) =
  S.foldl' (\acc (r, _) -> acc + numberOfRows - r) 0 settled

-- part 2

part2 :: RockState -> Int
part2 = go M.empty 0
  where
    go seen iteration rockState = case seen M.!? settled rockState of
      Just prev
        | (s : _) <- M.keys (M.filter (== indexOfTarget prev iteration) seen) ->
            power (rockState {settled = s})
        | otherwise -> error "oopsie daisies"
      Nothing ->
        let seen' = M.insert (settled rockState) iteration seen
         in go seen' (iteration + 1) (tiltCycle rockState)
    indexOfTarget start end = start + ((target - end) `mod` (end - start))
    target = 1000000000 :: Int

tiltCycle :: RockState -> RockState
tiltCycle currentState =
  currentState & tilt north & tilt west & tilt south & tilt east
