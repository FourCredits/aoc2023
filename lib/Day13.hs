module Day13 (parse, solve, part1, part2) where

import Data.Array (Array, bounds, ixmap, listArray, (!))
import qualified Text.Parsec as P

type Pattern = Array (Int, Int) Tile

data Tile = Ash | Rock deriving (Eq)

data Mirror = Horizontal Int | Vertical Int deriving (Show, Eq)

type MirrorPredicate = Pattern -> Int -> Bool

solve :: Text -> (Text, Text)
solve = (display part1 &&& display part2) . parse
  where
    display f = either (const "parse error") (Prelude.show . f)

parse :: Text -> Either P.ParseError [Pattern]
parse = P.parse (P.sepBy pattern P.newline) ""
  where
    pattern = P.sepEndBy line P.newline >>= makePattern
    line = P.many1 $ (P.char '.' $> Ash) <|> (P.char '#' $> Rock)
    makePattern tiles@(firstRow@(_ : _) : _) =
      pure $
        listArray ((0, 0), (length tiles - 1, length firstRow - 1)) $
          concat tiles
    makePattern ([] : _) = fail "rows must have at least one element"
    makePattern _ = fail "must have at least one row"

part1, part2 :: [Pattern] -> Maybe Int
part1 = summarizeNotes isExactlyMirroredAt
part2 = summarizeNotes isAlmostMirroredAt

summarizeNotes :: MirrorPredicate -> [Pattern] -> Maybe Int
summarizeNotes predicate =
  fmap combineOrientations . traverse (findMirror predicate)
  where
    combineOrientations mirrors =
      let (horizontals, verticals) = foldr f (0, 0) mirrors
       in (horizontals * 100 + verticals)
    f (Horizontal n) (horizontals, verticals) = (horizontals + n, verticals)
    f (Vertical n) (horizontals, verticals) = (horizontals, verticals + n)

isExactlyMirroredAt :: MirrorPredicate
isExactlyMirroredAt pattern start =
  all columnsAreMirrored $ zip leftSide rightSide
  where
    columnsAreMirrored = uncurry ((==) `on` column)
    column j = [pattern ! (i, j) | i <- [lowRow .. highRow]]
    rightSide = [start + 1 .. highColumn]
    leftSide = [start, start - 1 .. lowColumn]
    ((lowRow, lowColumn), (highRow, highColumn)) = bounds pattern

isAlmostMirroredAt :: MirrorPredicate
isAlmostMirroredAt pattern start =
  filter (/= 0) (zipWith countDifferences leftSide rightSide) == [1]
  where
    countDifferences c1 c2 =
      length $ filter id $ zipWith (/=) (column c1) (column c2)
    column j = [pattern ! (i, j) | i <- [lowRow .. highRow]]
    rightSide = [start + 1 .. highColumn]
    leftSide = [start, start - 1 .. lowColumn]
    ((lowRow, lowColumn), (highRow, highColumn)) = bounds pattern

findMirror :: MirrorPredicate -> Pattern -> Maybe Mirror
findMirror pre pat = findVerticalMirror pre pat <|> findHorizontalMirror pre pat

findVerticalMirror :: MirrorPredicate -> Pattern -> Maybe Mirror
findVerticalMirror isMirroredAt pattern =
  [lowColumn .. highColumn - 1]
    & find (pattern `isMirroredAt`)
    & fmap (Vertical . (+ 1))
  where
    ((_, lowColumn), (_, highColumn)) = bounds pattern

findHorizontalMirror :: MirrorPredicate -> Pattern -> Maybe Mirror
findHorizontalMirror predicate =
  fmap makeHorizontal . findVerticalMirror predicate . rotateAnticlockwise
  where
    makeHorizontal (Vertical n) = Horizontal n
    makeHorizontal (Horizontal n) = Horizontal n -- unreachable

rotateAnticlockwise :: Array (Int, Int) a -> Array (Int, Int) a
rotateAnticlockwise array = ixmap bounds' (\(r, c) -> (c, hiC - r)) array
  where
    ((loR, loC), (hiR, hiC)) = bounds array
    bounds' = ((loC, loR), (hiC, hiR))
