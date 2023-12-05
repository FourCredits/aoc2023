{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day05 (parse, solve, part1, part2, Map, Range (..)) where

import Control.Applicative (asum)
import Control.Monad (void)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (foldl')
import Data.Maybe (fromMaybe, mapMaybe)
import Debug.Trace (traceShow)
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
      f ss m = case traceShow (ss, m) $ concatMap (convertSpan m) ss of
        [] -> ss
        result -> result
      finalSpans = foldl' f spans maps
   in finalSpans <&> fst & minimum

type Span = (Int, Int)

-- TODO: be a little cleverer: you have to break off chunks of the span, then
-- just return what's left once you've exhausted the ranges

convertSpan :: Map -> Span -> [Span]
convertSpan ranges (lo, hi) = ranges & filter relevant & mapMaybe extractSubSpan
  where
    extractSubSpan (Range dS sS rL) =
      if loInDest > hiInDest then Nothing else Just (loInDest, hiInDest)
      where
        loInDest = dS + max 0 (lo - sS)
        hiInDest = dS + min rL (hi - sS)
    relevant (Range _ sS rL) = sS <= hi && (sS + rL) >= lo

makeSpans :: [Int] -> [Span]
makeSpans = go []
  where
    go acc [] = reverse acc
    go acc (x : y : ys) = go ((x, x + y - 1) : acc) ys
    go _ _ = error "odd number of elements in list"
