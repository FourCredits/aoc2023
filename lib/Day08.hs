module Day08 (solve, part1, part2, parse, Direction (..), Input) where

import Data.Array (Array, listArray, (!))
import Data.Char (isAlphaNum)
import Data.List (elemIndex, findIndex, foldl1', iterate')
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Text.Parsec as P

data Direction = L | R deriving (Eq, Show)

type Network = M.Map Text (Text, Text)

type Input = (Array Int Direction, Network)

solve :: Text -> (Text, Text)
solve = (display part1 &&& display part2) . parse
  where
    display f = either (const "parse error") (maybe "failed" show . f)

parse :: Text -> Either P.ParseError Input
parse = P.parse parser ""
  where
    parser = sepPair (P.count 2 P.newline) directions nodes
    sepPair separator a b = (,) <$> a <*> (separator *> b)
    directions = P.many direction <&> toArray
    toArray elems = listArray (0, length elems - 1) elems
    direction = (P.char 'L' $> L) <|> (P.char 'R' $> R)
    nodes = M.fromList <$> P.sepEndBy node P.newline
    node = sepPair (P.string " = ") word pair
    pair =
      P.between (P.char '(') (P.char ')') (sepPair (P.string ", ") word word)
    word = P.many (P.satisfy isAlphaNum) <&> toText

part1 :: Input -> Maybe Int
part1 input = elemIndex "ZZZ" $ nodeSequence input "AAA"

part2 :: Input -> Maybe Int
part2 input@(_, nodes) = do
  starts <- nodes & M.keys & filter (`endsIn` 'A') & nonEmpty
  loops <- traverse (findIndex (`endsIn` 'Z') . nodeSequence input) starts
  pure $ foldl1' lcm $ toList loops

nodeSequence :: Input -> Text -> [Text]
nodeSequence (directions, nodes) start = map snd $ iterate' next (0, start)
  where
    next (index, node) =
      ((index + 1) `mod` numDirections, nextStep (directions ! index) node)
    nextStep L position = fst $ nodes M.! position
    nextStep R position = snd $ nodes M.! position
    numDirections = length directions

endsIn :: Text -> Char -> Bool
endsIn string char = one char `T.isSuffixOf` string
