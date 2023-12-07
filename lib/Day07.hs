{-# LANGUAGE FlexibleContexts #-}

module Day07 (parse, Card (..), Hand (..), HandType, part1) where

import Control.Applicative (asum)
import Data.Function (on)
import Data.Functor (($>), (<&>))
import Data.List (group, sort, sortOn)
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
  deriving (Show, Eq, Ord)

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
