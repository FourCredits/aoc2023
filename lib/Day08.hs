module Day08 (solve, part1, part2, parse, Direction (..), Input) where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Data.Array (Array, listArray, (!))
import Data.Char (isAlphaNum)
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.List (elemIndex, findIndex, foldl1', iterate')
import Data.List.NonEmpty (nonEmpty, toList)
import qualified Data.Map as M
import qualified Text.Parsec as P

data Direction = L | R deriving (Eq, Show)

type Network = M.Map String (String, String)

type Input = (Array Int Direction, Network)

solve :: String -> (String, String)
solve = (display part1 &&& display part2) . parse
  where
    display f = either (const "parse error") (maybe "failed" show . f)

parse :: String -> Either P.ParseError Input
parse = P.parse parser ""
  where
    parser = sepPair (P.count 2 P.newline) directions nodes
    sepPair separator first second = (,) <$> first <*> (separator *> second)
    directions = P.many direction <&> toArray
    toArray elems = listArray (0, length elems - 1) elems
    direction = (P.char 'L' $> L) <|> (P.char 'R' $> R)
    nodes = M.fromList <$> P.sepEndBy node P.newline
    node = sepPair (P.string " = ") word pair
    pair =
      P.between (P.char '(') (P.char ')') (sepPair (P.string ", ") word word)
    word = P.many (P.satisfy isAlphaNum)

part1 :: Input -> Maybe Int
part1 input = elemIndex "ZZZ" $ nodeSequence input "AAA"

part2 :: Input -> Maybe Int
part2 input@(_, nodes) = do
  starts <- nodes & M.keys & filter (endsIn 'A') & nonEmpty
  loops <- traverse (findIndex (endsIn 'Z') . nodeSequence input) starts
  pure $ foldl1' lcm $ toList loops

nodeSequence :: Input -> String -> [String]
nodeSequence (directions, nodes) start = map snd $ iterate' next (0, start)
  where
    next (index, node) =
      ((index + 1) `mod` numDirections, nextStep (directions ! index) node)
    nextStep L position = fst $ nodes M.! position
    nextStep R position = snd $ nodes M.! position
    numDirections = length directions

endsIn :: (Eq a) => a -> [a] -> Bool
endsIn _ [] = False
endsIn x ys = x == last ys
