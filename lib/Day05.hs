{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day05 (parse, solve, part1, part2, Map, Range (..)) where

import Control.Applicative (asum)
import Control.Monad (void)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import qualified Text.Parsec as P

type Map = [Range]

data Range = Range
  { destinationStart :: Int,
    sourceStart :: Int,
    rangeLength :: Int
  }
  deriving (Show, Eq)

parse :: String -> Either P.ParseError ([Int], [Map])
parse = P.parse parser ""
  where
    parser = (,) <$> (seedsP <* sep) <*> P.sepBy mapP P.newline
    seedsP = P.string "seeds: " *> intListP
    intListP = P.sepBy intP spaceP
    spaceP = P.char ' '
    intP = P.many P.digit <&> read
    sep = void $ P.count 2 P.newline
    mapP =
      P.many (P.satisfy (/= '\n')) *> P.newline *> P.sepEndBy rangeP P.newline
    rangeP =
      intListP >>= \case
        [dS, sS, rL] -> pure $ Range dS sS rL
        _ -> fail "wrong number of numbers"

solve :: String -> (String, String)
solve _ = ("unimplemented", "unimplemented")

part1 :: ([Int], [Map]) -> Int
part1 (seeds, maps) = seeds <&> (\seed -> foldl' convert seed maps) & minimum

convert :: Int -> Map -> Int
convert source categoryMap =
  categoryMap
    <&> mapRange source
    & asum
    & fromMaybe source

mapRange :: Int -> Range -> Maybe Int
mapRange source (Range {destinationStart, sourceStart, rangeLength})
  | sourceStart <= source && source < (sourceStart + rangeLength) =
      Just (destinationStart + (source - sourceStart))
  | otherwise = Nothing

part2 :: ([Int], [Map]) -> Int
part2 (seedRanges, maps) =
  let spans = makeSpans seedRanges
      -- f ss m = case concatMap (convertSpan2 m) ss of
      f ss m = case concatMap (convertSpan2 m) ss of
        [] -> ss
        result -> result
      finalSpans = foldl' f spans maps
   in finalSpans <&> fst & minimum

type Span = (Int, Int)

-- TODO: be a little cleverer: you have to break off chunks of the span, then
-- just return what's left once you've exhausted the ranges

-- convertSpan :: Map -> Span -> [Span]
-- convertSpan ranges (lo, hi) = ranges & filter relevant & mapMaybe extractSubSpan
--   where
--     extractSubSpan (Range dS sS rL) =
--       if loInDest > hiInDest then Nothing else Just (loInDest, hiInDest)
--       where
--         loInDest = dS + max 0 (lo - sS)
--         hiInDest = dS + min rL (hi - sS)
--     relevant (Range _ sS rL) = sS <= hi && (sS + rL) >= lo

convertSpan2 :: Map -> Span -> [Span]
-- if no part of the map matches a number, then it's passed through as is
convertSpan2 [] inputSpan = [inputSpan]
-- if all of the input is matched, no need to keep going
convertSpan2 ((Range dS sS rL) : rs') (l, h)
  -- input span and source range don't overlap at all. continue to next
  | h < sS || l > sE = convertSpan2 rs' (l, h)
  -- input span is entirely inside source range
  | l >= sS && h <= sE =
      [centreSpan]
  -- input span has overhang on both sides
  | l < sS && h > sE =
      let lowResult = convertSpan2 rs' lowOverhang
          highResult = convertSpan2 rs' highOverhang
       in centreSpan : lowResult <> highResult
  -- input span has overhang on low side
  | l < sS && h <= sE = centreSpan : convertSpan2 rs' lowOverhang
  -- input span has overhang on high side
  | l >= sS && h > sE = centreSpan : convertSpan2 rs' highOverhang
  | otherwise = error "unreachable"
  where
    sE = sS + rL - 1
    centreSpan = (dS + max (l - sS) 0, dS + min (h - sS) (rL - 1))
    lowOverhang = (l, sS - 1)
    highOverhang = (sE + 1, h)
    convertIfValid s@(lo, hi)
      | lo <= hi = convertSpan2 rs' s
      | otherwise = []

makeSpans :: [Int] -> [Span]
makeSpans = go []
  where
    go acc [] = reverse acc
    go acc (x : y : ys) = go ((x, x + y - 1) : acc) ys
    go _ _ = error "odd number of elements in list"
