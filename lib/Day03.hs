{-# LANGUAGE ViewPatterns #-}

module Day03 where

-- module Day03 (solve, part1, part2) where

import Data.Array
import Data.Char (isDigit)
import Data.Function ((&))

solve :: String -> (String, String)
solve _ = ("unimplemented", "unimplemented")

part1 :: String -> Int
part1 (parse -> (field, potentialParts)) =
  potentialParts & filter (isPart field) & map (lookupValue field) & sum

lookupValue :: Field -> Part -> Int
lookupValue field (Part r cs ce) = read $ [field ! (r, c) | c <- [cs .. ce]]

isSymbol :: Char -> Bool
isSymbol c = (c /= '.') && not (isDigit c)

type Position = (Int, Int)

type Field = Array Position Char

data Part = Part Int Int Int

isPart :: Field -> Part -> Bool
isPart field (Part r cs ce) = any isSymbol $ do
  c <- [cs .. ce]
  neighbours field (r, c)

neighbours :: Field -> Position -> [Char]
neighbours field (r, c) = positions & filter (inBounds bounds_) & map (field !)
  where
    positions =
      [ (r + 1, c - 1),
        (r + 1, c),
        (r + 1, c + 1),
        (r, c - 1),
        (r, c + 1),
        (r - 1, c - 1),
        (r - 1, c),
        (r - 1, c + 1)
      ]
    bounds_ = bounds field

inBounds :: (Position, Position) -> Position -> Bool
inBounds ((loR, loC), (hiR, hiC)) (r, c) =
  loR <= r && r <= hiR && loC <= c && c <= hiC

parse :: String -> (Field, [Part])
parse input =
  let ls = input & lines
      parts = do
        (row_, string) <- enumerate ls
        (colStart_, colEnd_) <- findPotentialParts string
        return $ Part row_ colStart_ colEnd_
      numRows = length ls
      numCols = length (head ls)
      field = listArray ((0, 0), (numRows - 1, numCols - 1)) (concat ls)
   in (field, parts)

findPotentialParts :: String -> [(Int, Int)]
findPotentialParts input = go Nothing [] (enumerate input)
  where
    go Nothing acc ((position, c) : cs)
      | isDigit c = go (Just (position, position)) acc cs
      | otherwise = go Nothing acc cs
    go (Just active@(start, _)) acc ((position, c) : cs)
      | isDigit c = go (Just (start, position)) acc cs
      | otherwise = go Nothing (active : acc) cs
    go Nothing acc [] = acc
    go (Just active) acc [] = active : acc

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

part2 :: String -> Int
part2 = undefined
