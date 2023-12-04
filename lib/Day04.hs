{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Day04 (solve, part1, part2, parse, Card (..)) where

import Data.Array
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Set as S
import qualified Text.Parsec as P

data Card = Card
  { winning :: S.Set Int,
    present :: S.Set Int
  }
  deriving (Show, Eq)

solve :: String -> (String, String)
solve input = case parse input of
  Right cards -> (show $ part1 cards, show $ part2 cards)
  _ -> ("parse error", "parse error")

parse :: String -> Either P.ParseError [Card]
parse = P.parse (P.sepEndBy gameP P.newline) ""
  where
    gameP = Card <$> winningP <*> presentP
    winningP = P.string "Card " *> intP *> P.string ": " *> setP
    presentP = P.string "| " *> setP
    intP = P.many P.space *> P.many1 P.digit <&> read
    setP = P.sepEndBy intP (P.char ' ') <&> S.fromList

part1 :: [Card] -> Int
part1 = sum . map score

score :: Card -> Int
score card = case countWins card of
  0 -> 0
  n -> 2 ^ (n - 1)

countWins :: Card -> Int
countWins (Card {winning, present}) = S.intersection winning present & S.size

part2 :: [Card] -> Int
part2 cards = foldl' generateTickets start [1 .. hi] & elems <&> snd & sum
  where
    generateTickets tickets i =
      let (card, amount) = tickets ! i
          wins = countWins card
          updateRange = [i + 1 .. min (i + wins) hi]
          update j = let (c, a) = tickets ! j in (j, (c, a + amount))
          updates = map update updateRange
       in (tickets // updates)
    start = cards & map (,1) & zip [1 ..] & array (1, length cards)
    (_, hi) = bounds start
