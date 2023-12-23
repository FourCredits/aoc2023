{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}

module Day15 (part1, part2, solve) where

import Data.Char (isDigit, isLetter, isSpace)
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Text as T

type Label = Text

type FocalLength = Int

type Box = Seq (Label, FocalLength)

data Action = Insert Label FocalLength | Remove Label deriving (Show)

solve :: Text -> (Text, Text)
solve = (show . part1) &&& (maybe "parse error" show . part2)

parse :: Text -> [Text]
parse input = input & T.dropWhileEnd isSpace & T.split (== ',')

(#) :: (a -> b) -> (b -> c) -> a -> c
(#) = flip (.)

part1 :: Text -> Int
part1 = parse # map hash # sum

hash :: Text -> Int
hash = T.foldl' (\acc c -> (17 * (acc + ord c)) `mod` 256) 0

part2 :: Text -> Maybe Int
part2 = parse # traverse parseStep # fmap (executeActions # focusingPower)

executeActions :: [Action] -> Map Int Box
executeActions = foldl' executeAction M.empty
  where
    executeAction acc (Remove label) =
      M.adjust (S.filter ((/= label) . fst)) (hash label) acc
    executeAction acc (Insert label focalLength) =
      let remove box = case S.findIndexL ((== label) . fst) box of
            Nothing -> box S.:|> (label, focalLength)
            Just index -> S.update index (label, focalLength) box
       in M.alter (pure . maybe [(label, focalLength)] remove) (hash label) acc

focusingPower :: Map Int Box -> Int
focusingPower m = sum $ do
  (box, lenses) <- M.assocs m
  (slotNumber, focalLength) <- zip [1 ..] (snd <$> toList lenses)
  pure ((box + 1) * slotNumber * focalLength)

parseStep :: Text -> Maybe Action
parseStep (T.break (not . isLetter) -> (label, rest)) = case toString rest of
  "-" -> pure (Remove label)
  ['=', c] | isDigit c -> pure (Insert label (ord c - ord '0'))
  _ -> Nothing
