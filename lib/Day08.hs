module Day08 (solve, part1, parse, Direction (..), Input) where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Monad (foldM)
import Data.Char (isLetter)
import Data.Functor (($>))
import qualified Data.Map as M
import qualified Text.Parsec as P

data Direction = L | R deriving (Eq, Show)

type Input = ([Direction], M.Map String (String, String))

solve :: String -> (String, String)
solve = (error "unimplemented" &&& error "unimplemented") . parse

parse :: String -> Either P.ParseError Input
parse = P.parse parser ""
  where
    parser = (,) <$> directionsP <*> (P.count 2 P.newline *> nodesP)
    directionsP = P.many directionP
    directionP = (P.char 'L' $> L) <|> (P.char 'R' $> R)
    nodesP = M.fromList <$> P.sepEndBy nodeP P.newline
    nodeP =
      (\key left right -> (key, (left, right)))
        <$> wordP
        <*> (P.string " = (" *> wordP)
        <*> (P.string ", " *> wordP <* P.string ")")
    wordP = P.many (P.satisfy isLetter)

part1 :: Input -> Int
part1 (directions, nodes) = go 0 "AAA" $ cycle directions
  where
    go _ _ [] = error "unreachable"
    go steps "ZZZ" _ = steps
    go steps position (dir : dirs) =
      case (dir, nodes M.!? position) of
        (_, Nothing) -> error $ "unknown position " <> position
        (L, Just (left, _)) -> go (steps + 1) left dirs
        (R, Just (_, right)) -> go (steps + 1) right dirs
