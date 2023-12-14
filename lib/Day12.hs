module Day12 (part1) where

import Control.Applicative (empty)
import Control.Arrow (first)

data Record = Operational | Damaged deriving (Eq)

instance Show Record where
  show Operational = "."
  show Damaged = "#"

part1 :: String -> Maybe Int
part1 input = sum . map (uncurry arrangements . first records) <$> parse input

arrangements :: [[Record]] -> [Int] -> Int
arrangements rs ns = length $ filter ((== ns) . countDamages) rs

countDamages :: [Record] -> [Int]
countDamages = finish . foldr f ([], Nothing)
  where
    finish (numbers, final) = maybe numbers (: numbers) final
    f Operational (acc, Just current) = (current : acc, Nothing)
    f Operational (acc, Nothing) = (acc, Nothing)
    f Damaged (acc, Just current) = (acc, Just (current + 1))
    f Damaged (acc, Nothing) = (acc, Just 1)

parse :: String -> Maybe [(String, [Int])]
parse = traverse parseLine . lines
  where
    parseLine input = case words input of
      [rs, ns] -> pure (rs, map read $ splitByComma ns)
      _ -> empty

splitByComma :: String -> [String]
splitByComma [] = []
splitByComma (',' : rest) = "" : splitByComma rest
splitByComma (c : rest) = case splitByComma rest of
  [] -> [[c]]
  (d : ds) -> (c : d) : ds

records :: String -> [[Record]]
records [] = [[]]
records ('#' : rest) = map (Damaged :) $ records rest
records ('.' : rest) = map (Operational :) $ records rest
records ('?' : rest) = records ('#' : rest) ++ records ('.' : rest)
records _ = error "bad input"
