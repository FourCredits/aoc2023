module Day02 (solve, part1, part2, parseInput, Game) where

import qualified Text.Parsec as P

solve :: Text -> (Text, Text)
solve input = case parseInput input of
  Left problem -> (show problem, show problem)
  Right parsed -> (show $ part1 parsed, show $ part2 parsed)

parseInput :: Text -> Either P.ParseError [Game]
parseInput = P.parse inputParser ""

part1, part2 :: [Game] -> Int
part1 = sum . map gameId . filter (gameIsPossible target)
  where
    target = Set {reds = 12, greens = 13, blues = 14}
part2 = sum . map (power . fold . revealedSets)

data Game = Game {gameId :: Int, revealedSets :: [Balls]}

data Balls = Set {reds :: Int, greens :: Int, blues :: Int}

instance Semigroup Balls where
  (Set r1 g1 b1) <> Set r2 g2 b2 = Set (max r1 r2) (max g1 g2) (max b1 b2)

instance Monoid Balls where
  mempty = Set 0 0 0

power :: Balls -> Int
power (Set r g b) = product [r, g, b]

gameIsPossible :: Balls -> Game -> Bool
gameIsPossible (Set inRed inGreen inBlue) (Game _ sets) =
  let (Set maxRed maxGreen maxBlue) = fold sets
   in inRed >= maxRed && inGreen >= maxGreen && inBlue >= maxBlue

inputParser :: P.Parsec Text u [Game]
inputParser = P.sepEndBy gameParser P.newline
  where
    gameParser = Game <$> idParser <*> P.sepBy setParser (P.string "; ")
    idParser = P.string "Game " *> intParser <* P.string ": "
    setParser =
      P.sepBy cubeParser (P.string ", ") <&> \table ->
        let check color = table & find ((== color) . fst) & maybe 0 snd
         in Set (check "red") (check "green") (check "blue")
    cubeParser = flip (,) <$> intParser <*> (P.char ' ' *> colorParser)
    colorParser = asum (map P.string ["red", "green", "blue"])
    intParser =
      many P.digit >>= \intString -> case readEither intString of
        Right int -> pure int
        Left reason -> fail $ toString reason
