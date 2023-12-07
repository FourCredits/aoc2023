{-# LANGUAGE FlexibleContexts #-}

module Day07 (solve, parse, Card (..), Hand (..), HandType, part1, part2) where

import Control.Applicative (asum)
import Control.Arrow ((&&&))
import Data.Function (on)
import Data.Functor (($>), (<&>))
import Data.List (delete, group, sort, sortOn)
import qualified Text.Parsec as P

data Card
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | T
  | J
  | Q
  | K
  | A
  deriving (Show, Eq, Ord, Enum)

newtype Hand = Hand {cards :: [Card]} deriving (Eq, Show)

instance Ord Hand where
  compare = (compare `on` handType) `thenOn` cards

data HandType
  = OneOfAKind
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Show, Eq, Ord)

solve :: String -> (String, String)
solve = ((display . fmap part1) &&& (display . fmap part2)) . parse
  where
    display = either (const "parse error") show

handType :: Hand -> HandType
handType = go . sort . map length . group . sort . cards
  where
    go [5] = FiveOfAKind
    go [1, 4] = FourOfAKind
    go [2, 3] = FullHouse
    go [1, 1, 3] = ThreeOfAKind
    go [1, 2, 2] = TwoPair
    go [1, 1, 1, 2] = OnePair
    go [1, 1, 1, 1, 1] = OneOfAKind
    go _ = error "invalid hand"

thenOn :: (Ord b) => (a -> a -> Ordering) -> (a -> b) -> a -> a -> Ordering
thenOn comp mapping a1 a2 = case comp a1 a2 of
  EQ -> (compare `on` mapping) a1 a2
  notEq -> notEq

parse :: String -> Either P.ParseError [(Hand, Int)]
parse = P.parse parser ""
  where
    parser = P.sepEndBy handAndBidP P.newline
    handAndBidP = (,) <$> handP <*> (P.space *> intP)
    handP = P.many cardP <&> Hand
    cardP = asum $ map makeCard cardMappings
    makeCard (character, card) = P.char character $> card
    cardMappings =
      [ ('A', A),
        ('K', K),
        ('Q', Q),
        ('J', J),
        ('T', T),
        ('9', Nine),
        ('8', Eight),
        ('7', Seven),
        ('6', Six),
        ('5', Five),
        ('4', Four),
        ('3', Three),
        ('2', Two)
      ]
    intP = P.many P.digit <&> read

part1 :: [(Hand, Int)] -> Int
part1 = sum . zipWith winnings [1 ..] . sortOn fst

winnings :: Int -> (a, Int) -> Int
winnings rank (_, bet) = bet * rank

part2 :: [(Hand, Int)] -> Int
part2 = sum . zipWith winnings [1 ..] . sortOn (handWithJokers . fst)

newtype HandWithJokers = HandWithJokers
  { cardsWithJokers :: [CardWithJokers]
  }
  deriving (Show, Eq)

instance Ord HandWithJokers where
  compare = (compare `on` handTypeWithJokers) `thenOn` cardsWithJokers

handWithJokers :: Hand -> HandWithJokers
handWithJokers = HandWithJokers . map CardWithJokers . cards

handTypeWithJokers :: HandWithJokers -> HandType
handTypeWithJokers = maximum . map (handType . Hand) . evaluateHands . cardsWithJokers

evaluateHands :: [CardWithJokers] -> [[Card]]
evaluateHands [] = [[]]
evaluateHands ((CardWithJokers J) : rest) =
  (:) <$> delete J [Two .. A] <*> evaluateHands rest
evaluateHands ((card : rest)) = map (getCard card :) (evaluateHands rest)

newtype CardWithJokers = CardWithJokers {getCard :: Card} deriving (Show, Eq)

instance Ord CardWithJokers where
  compare (CardWithJokers a) (CardWithJokers b) = case (a, b) of
    (J, J) -> EQ
    (_, J) -> GT
    (J, _) -> LT
    (x, y) -> compare x y
