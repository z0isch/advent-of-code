module Day24 where

import           Data.List


partOne = undefined

weightNeeded = fromIntegral (sum input) / 3

input :: [Integer]
input = map read $ words "1 3 5 11 13 17 19 23 29 31 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113"
