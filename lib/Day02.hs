module Day02 (solve, part1, part2, parseInput, Game) where

import Control.Applicative (asum)
import Data.Foldable (fold)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Text.Parsec

solve :: String -> (String, String)
solve input = case parseInput input of
  Left problem -> (show problem, show problem)
  Right parsed -> (show $ part1 parsed, show $ part2 parsed)

parseInput :: String -> Either ParseError [Game]
parseInput = parse inputParser ""

part1, part2 :: [Game] -> Int
part1 = sum . map gameId . filter (gameIsPossible target)
  where
    target = Set {reds = 12, greens = 13, blues = 14}
part2 = sum . map (power . fold . revealedSets)

data Game = Game {gameId :: Int, revealedSets :: [Set]}

data Set = Set {reds :: Int, greens :: Int, blues :: Int}

instance Semigroup Set where
  (Set r1 g1 b1) <> Set r2 g2 b2 = Set (max r1 r2) (max g1 g2) (max b1 b2)

instance Monoid Set where
  mempty = Set 0 0 0

power :: Set -> Int
power (Set r g b) = product [r, g, b]

gameIsPossible :: Set -> Game -> Bool
gameIsPossible (Set inRed inGreen inBlue) (Game _ sets) =
  let (Set maxRed maxGreen maxBlue) = fold sets
   in inRed >= maxRed && inGreen >= maxGreen && inBlue >= maxBlue

inputParser :: Parsec String u [Game]
inputParser = sepEndBy gameParser newline
  where
    gameParser = Game <$> idParser <*> sepBy setParser (string "; ")
    idParser = string "Game " *> intParser <* string ": "
    setParser =
      sepBy cubeParser (string ", ") <&> \table ->
        let check color = fromMaybe 0 $ lookup color table
         in Set (check "red") (check "green") (check "blue")
    cubeParser = flip (,) <$> intParser <*> (char ' ' *> colorParser)
    colorParser = asum (map string ["red", "green", "blue"])
    intParser = many digit <&> read
