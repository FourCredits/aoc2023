{-# LANGUAGE FlexibleContexts #-}

module Day07 (solve, parse, Card (..), Hand (..), HandType, part1, part2) where

import Data.List (maximumBy, partition)
import qualified Data.List.NonEmpty as NE
import qualified Text.Parsec as P

-- common

solve :: Text -> (Text, Text)
solve = ((display . fmap part1) &&& (display . fmap part2)) . parse
  where
    display = either (const "parse error") show

parse :: Text -> Either P.ParseError [(Hand, Int)]
parse = P.parse parser ""
  where
    parser = P.sepEndBy handAndBidP P.newline
    handAndBidP = (,) <$> handP <*> (P.space *> intP)
    intP = P.many P.digit >>= maybe empty pure . readMaybe
    handP = P.count 5 cardP <&> Hand
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

-- part 1: without jokers

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
  compare = compare `on` (handType &&& cards)

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

part1 :: [(Hand, Int)] -> Int
part1 = sum . zipWith winnings [1 ..] . sortWith fst

winnings :: Int -> (a, Int) -> Int
winnings rank (_, bet) = bet * rank

-- part 2: handling jokers

newtype CardWithJokers = CardWithJokers {getCard :: Card} deriving (Show, Eq)

instance Ord CardWithJokers where
  compare (CardWithJokers a) (CardWithJokers b) = case (a, b) of
    (J, J) -> EQ
    (_, J) -> GT
    (J, _) -> LT
    (x, y) -> compare x y

newtype HandWithJokers = HandWithJokers
  { cardsWithJokers :: [CardWithJokers]
  }
  deriving (Show, Eq)

instance Ord HandWithJokers where
  compare = compare `on` (handTypeWithJokers &&& cardsWithJokers)

handTypeWithJokers :: HandWithJokers -> HandType
handTypeWithJokers (HandWithJokers hand) =
  maybe hand replaceJokers (viaNonEmpty mode others) & toHand & handType
  where
    toHand = Hand . map getCard
    replaceJokers = (<> others) . replicate (length jokers)
    (jokers, others) = partition ((== J) . getCard) hand

mode :: (Ord a) => NonEmpty a -> a
mode elements =
  elements & NE.sort & NE.group1 & maximumBy (comparing length) & head

handWithJokers :: Hand -> HandWithJokers
handWithJokers = HandWithJokers . map CardWithJokers . cards

part2 :: [(Hand, Int)] -> Int
part2 = sum . zipWith winnings [1 ..] . sortOn (handWithJokers . fst)
