module Day18 (parse, part1, Instruction (..), Direction (..)) where

import Data.Array (inRange, range)
import Data.Char (isDigit)
import Data.List (maximum, minimum, (\\))
import qualified Data.Set as S
import qualified Text.Parsec as P

data Direction = D | U | L | R deriving (Show, Eq)

type ColorChannel = Int

data Instruction
  = Instruction Direction Int ColorChannel ColorChannel ColorChannel
  deriving (Show, Eq)

type Pos = (Int, Int)

parse :: Text -> Either P.ParseError [Instruction]
parse = P.parse (P.sepEndBy instruction P.newline) ""
  where
    instruction = do
      d <- direction
      distance <- P.char ' ' *> int
      (r, g, b) <- P.char ' ' *> P.between (P.char '(') (P.char ')') color
      pure $ Instruction d distance r g b
    color =
      P.char '#' *> ((,,) <$> colorChannel <*> colorChannel <*> colorChannel)
    direction =
      asum [P.char 'D' $> D, P.char 'U' $> U, P.char 'L' $> L, P.char 'R' $> R]
    int = P.many (P.satisfy isDigit) >>= parseInt
    colorChannel = combineHexDigits <$> P.count 2 hexDigit
    parseInt = maybe (fail "failed to parse int") pure . readMaybe
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

-- TODO: does that work?
part1 :: [Instruction] -> Int
part1 = S.size . fillIn . fst . foldl' f (one (0, 0), (0, 0))
  where
    f (boundary, position) (Instruction dir distance _ _ _) =
      let newPositions = move position dir distance
          boundary' = foldr S.insert boundary newPositions
          position' = fromMaybe position $ viaNonEmpty last newPositions
       in (boundary', position')

move :: (Int, Int) -> Direction -> Int -> [(Int, Int)]
move (r, c) D distance = [(r', c) | r' <- [r + 1 .. r + distance]]
move (r, c) U distance = [(r', c) | r' <- [r - 1, r - 2 .. r - distance]]
move (r, c) L distance = [(r, c') | c' <- [c - 1, c - 2 .. c - distance]]
move (r, c) R distance = [(r, c') | c' <- [c + 1 .. c + distance]]

fillIn :: Set Pos -> Set Pos
fillIn boundary =
  bounds
    & range
    & (\\ toList boundary)
    & map (tryFill bounds boundary)
    & asum
    & fromMaybe boundary
  where
    bounds = extrema boundary

tryFill :: (Pos, Pos) -> Set Pos -> Pos -> Maybe (Set Pos)
tryFill bounds boundary current = go False boundary [current]
  where
    go touchedEdge filledIn [] =
      if not touchedEdge then Just filledIn else Nothing
    go touchedEdge filledIn (p : ps)
      | S.member p filledIn = go touchedEdge filledIn ps
      | not (inRange bounds p) = go True filledIn ps
      | otherwise =
          let newPositions = filter (`S.notMember` filledIn) (neighbours p)
           in go touchedEdge (S.insert p filledIn) (newPositions <> ps)

neighbours :: Pos -> [Pos]
neighbours (r, c) = [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

extrema :: Set Pos -> (Pos, Pos)
extrema boundary = ((lowR, lowC), (highR, highC))
  where
    lowR = minimum $ S.map fst boundary
    highR = maximum $ S.map fst boundary
    lowC = minimum $ S.map snd boundary
    highC = maximum $ S.map snd boundary
