module Day12 (part1) where

import Control.Applicative (empty)
import Control.Arrow (first)

data Record = Operational | Damaged deriving (Eq)

instance Show Record where
  show Operational = "."
  show Damaged = "#"

part1 :: String -> Maybe Int
part1 input = case parse input of
  Just parsed -> Just $ sum $ map (uncurry arrangements . first records) parsed
  Nothing -> Nothing

arrangements :: [[Record]] -> [Int] -> Int
arrangements rs ns = length $ filter ((== ns) . countDamages) rs

countDamages :: [Record] -> [Int]
countDamages = go [] Nothing
  where
    go acc (Just current) [] = reverse (current : acc)
    go acc Nothing [] = reverse acc
    go acc (Just current) (Operational : rest) = go (current : acc) Nothing rest
    go acc Nothing (Operational : rest) = go acc Nothing rest
    go acc (Just current) (Damaged : rest) = go acc (Just (current + 1)) rest
    go acc Nothing (Damaged : rest) = go acc (Just 1) rest

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
