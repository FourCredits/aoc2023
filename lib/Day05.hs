{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Day05 (parse, solve, part1, part2, ConversionMap, Range (..)) where

import Data.List (minimum)
import qualified Text.Parsec as P

type ConversionMap = [Range]

data Range = Range
  { destinationStart :: Int,
    sourceStart :: Int,
    rangeLength :: Int
  }
  deriving (Show, Eq)

type Span = (Int, Int)

parse :: Text -> Either P.ParseError ([Int], [ConversionMap])
parse = P.parse parser ""
  where
    parser = (,) <$> (seedsP <* sep) <*> P.sepBy mapP P.newline
    seedsP = P.string "seeds: " *> intListP
    intListP = P.sepBy intP spaceP
    spaceP = P.char ' '
    intP = P.many P.digit >>= maybe empty pure . readMaybe
    sep = void $ P.count 2 P.newline
    mapP =
      P.many (P.satisfy (/= '\n')) *> P.newline *> P.sepEndBy rangeP P.newline
    rangeP =
      intListP >>= \case
        [dS, sS, rL] -> pure $ Range dS sS rL
        _ -> fail "wrong number of numbers"

solve :: Text -> (Text, Text)
solve = (display . fmap part1 &&& display . fmap part2) . parse
  where
    display = either (const "parse error") show

part1 :: ([Int], [ConversionMap]) -> Int
part1 (seeds, maps) = seeds <&> (\seed -> foldl' convert seed maps) & minimum

convert :: Int -> ConversionMap -> Int
convert source categoryMap =
  categoryMap <&> mapRange source & asum & fromMaybe source

mapRange :: Int -> Range -> Maybe Int
mapRange source (Range {destinationStart, sourceStart, rangeLength})
  | sourceStart <= source && source < (sourceStart + rangeLength) =
      Just (destinationStart + (source - sourceStart))
  | otherwise = Nothing

part2 :: ([Int], [ConversionMap]) -> Int
part2 (seedRanges, maps) =
  maps & foldl' convertCategory startingSpans <&> fst & minimum
  where
    startingSpans = makeSpans seedRanges
    convertCategory spans mapper = case concatMap (convertSpan mapper) spans of
      [] -> spans
      result -> result

convertSpan :: ConversionMap -> Span -> [Span]
convertSpan [] inputSpan = [inputSpan]
convertSpan ((Range dS sS rL) : rs') (l, h)
  | h < sS || l > sE = convertSpan rs' (l, h)
  | otherwise =
      centreSpan : convertIfValid lowOverhang <> convertIfValid highOverhang
  where
    sE = sS + rL - 1
    centreSpan = (dS + max (l - sS) 0, dS + min (h - sS) (rL - 1))
    lowOverhang = (l, sS - 1)
    highOverhang = (sE + 1, h)
    convertIfValid Invalid = []
    convertIfValid s = convertSpan rs' s

pattern Invalid :: (Ord a) => (a, a)
pattern Invalid <- (invalid -> True)

invalid :: (Ord a) => (a, a) -> Bool
invalid (lo, hi) = lo > hi

makeSpans :: [Int] -> [Span]
makeSpans = go []
  where
    go acc [] = reverse acc
    go acc (x : y : ys) = go ((x, x + y - 1) : acc) ys
    go _ _ = error "odd number of elements in list"
