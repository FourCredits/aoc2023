{-# LANGUAGE ViewPatterns #-}

module Day15 (part1, part2, solve) where

import Data.Char (isDigit, isLetter, isSpace)
import qualified Data.Map as M
import qualified Data.Text as T

type Label = Text

type FocalLength = Int

data Action = Insert Label FocalLength | Remove Label

solve :: Text -> (Text, Text)
solve = (show . part1) &&& (maybe "parse error" show . part2)

part1 :: Text -> Int
part1 input = input & T.dropWhileEnd isSpace & T.split (== ',') & map hash & sum

hash :: Text -> Int
hash = T.foldl' (\acc c -> (17 * (acc + ord c)) `mod` 256) 0

part2 :: Text -> Maybe Int
part2 input =
  input
    & T.dropWhileEnd isSpace
    & T.split (== ',')
    & traverse parseStep
    & fmap (focusingPower . foldl' f M.empty)
  where
    f acc (Remove label) = M.adjust (filter ((/= label) . fst)) (hash label) acc
    f acc (Insert label focalLength) = M.alter g (hash label) acc
      where
        g Nothing = Just [(label, focalLength)]
        g (Just list) = pure $ case find ((== label) . fst) list of
          Nothing -> list <> [(label, focalLength)]
          Just _ -> map h list
        h (l, fl) = if l == label then (l, focalLength) else (l, fl)

focusingPower :: M.Map Int [(Text, FocalLength)] -> Int
focusingPower m = sum $ do
  (box, lenses) <- M.assocs m
  (slotNumber, focalLength) <- zip [1 ..] $ map snd lenses
  pure ((box + 1) * slotNumber * focalLength)

parseStep :: Text -> Maybe Action
parseStep (T.break (not . isLetter) -> (label, rest)) = case toString rest of
  "-" -> pure (Remove label)
  ['=', c] | isDigit c -> pure (Insert label (ord c - ord '0'))
  _ -> Nothing
