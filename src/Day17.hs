module Day17 where

import           Data.List

partOne = length filtered
partTwo = length $ takeWhile (\c -> length c == minimumNum) sorted
  where
    sorted = sortOn length filtered
    minimumNum = length $ head sorted

filtered = filter (\i -> sum i == 150) $ subsequences input

input :: [Int]
input = map read $ words "50 44 11 49 42 46 18 32 26 40 21 7 18 43 10 47 36 24 22 40 "
