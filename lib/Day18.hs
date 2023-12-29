module Day18 (part1, part2, Instruction (..), Direction (..)) where

import Data.Array (listArray, (!))
import Data.Char (isHexDigit)
import qualified Text.Parsec as P

data Direction = D | U | L | R deriving (Show, Eq)

data Instruction = Instruction Direction Int deriving (Show, Eq)

type Pos = (Int, Int)

part1 :: Text -> Either P.ParseError Int
part1 = fmap areaOfLagoon . P.parse (P.sepEndBy instruction P.newline) ""
  where
    instruction = do
      d <- direction
      distance <- P.char ' ' *> int
      _ <- P.string " (#" *> P.count 6 (P.satisfy isHexDigit) *> P.char ')'
      pure $ Instruction d distance
    direction =
      asum [P.char 'D' $> D, P.char 'U' $> U, P.char 'L' $> L, P.char 'R' $> R]
    int = P.many P.digit >>= parseInt
    parseInt = maybe (fail "failed to parse int") pure . readMaybe

part2 :: Text -> Either P.ParseError Int
part2 = fmap areaOfLagoon . P.parse (P.sepEndBy instruction P.newline) ""
  where
    instruction = do
      _ <- P.oneOf "DULR" *> P.char ' ' *> P.many1 P.digit *> P.string " (#"
      distance <- combineHexDigits <$> P.count 5 hexDigit
      d <- direction
      _ <- P.char ')'
      pure $ Instruction d distance
    direction =
      asum [P.char '0' $> R, P.char '1' $> D, P.char '2' $> L, P.char '3' $> U]
    combineHexDigits = foldl' (\acc c -> acc * 16 + c) 0
    hexDigit =
      asum
        [ P.char '0' $> 0,
          P.char '1' $> 1,
          P.char '2' $> 2,
          P.char '3' $> 3,
          P.char '4' $> 4,
          P.char '5' $> 5,
          P.char '6' $> 6,
          P.char '7' $> 7,
          P.char '8' $> 8,
          P.char '9' $> 9,
          P.char 'A' $> 10,
          P.char 'B' $> 11,
          P.char 'C' $> 12,
          P.char 'D' $> 13,
          P.char 'E' $> 14,
          P.char 'F' $> 15,
          P.char 'a' $> 10,
          P.char 'b' $> 11,
          P.char 'c' $> 12,
          P.char 'd' $> 13,
          P.char 'e' $> 14,
          P.char 'f' $> 15
        ]

areaOfLagoon :: [Instruction] -> Int
areaOfLagoon instructions =
  areaOfLoop points - (boundary `div` 2) + 1 + boundary
  where
    (points, boundary, _) = foldl' f ([], 0, (0, 0)) instructions
    f (points_, boundary_, pos) (Instruction dir dist) =
      (pos : points_, boundary_ + genericLength (move pos dir dist), nextPoint dir dist pos)
    nextPoint D dist (r, c) = (r + dist, c)
    nextPoint U dist (r, c) = (r - dist, c)
    nextPoint L dist (r, c) = (r, c - dist)
    nextPoint R dist (r, c) = (r, c + dist)

-- copied over from day 10. TODO: extract to somewhere common
areaOfLoop :: [Pos] -> Int
areaOfLoop vertices = [1 .. len] & map trapezoid & sum & (`div` 2) & abs
  where
    trapezoid i = (r i + r (i + 1)) * (c i - c (i + 1))
    (rows, columns) = bimap toArray toArray $ unzip vertices
    r i = rows ! (i `mod` len)
    c i = columns ! (i `mod` len)
    len = length vertices
    toArray = listArray (0, len - 1)

move :: (Int, Int) -> Direction -> Int -> [(Int, Int)]
move (r, c) D distance = [(r', c) | r' <- [r + 1 .. r + distance]]
move (r, c) U distance = [(r', c) | r' <- [r - 1, r - 2 .. r - distance]]
move (r, c) L distance = [(r, c') | c' <- [c - 1, c - 2 .. c - distance]]
move (r, c) R distance = [(r, c') | c' <- [c + 1 .. c + distance]]
