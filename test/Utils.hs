module Utils where

import Paths_aoc2023
import Text.Printf (printf)

realInput :: Int -> IO String
realInput day = getDataFileName (printf "inputs/day%02d.txt" day) >>= readFile
