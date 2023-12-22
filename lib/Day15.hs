module Day15 (part1) where

import Data.Char
import qualified Data.Text as T

part1 :: Text -> Int
part1 input = input & T.dropWhileEnd isSpace & T.split (== ',') & map hash & sum

hash :: Text -> Int
hash = T.foldl' (\acc c -> (17 * (acc + ord c)) `mod` 256) 0
