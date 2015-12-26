module Day24 where

import           Control.Monad
import           Data.Function
import           Data.HashSet  (HashSet)
import qualified Data.HashSet  as H
import           Data.List

partOne = product $ head $ sortOn product $ head $
  groupBy ((==) `on` length) $
  sortOn length $
  filter (\xs -> genericSum xs == weightNeeded 3 && g xs 3) $ subsequences input

partTwo = product $ head $ sortOn product $ head $
  groupBy ((==) `on` length) $
  sortOn length $
  filter (\xs -> genericSum xs == weightNeeded 4 && g2 xs) $ subsequences input

g xs i = any (\ys -> genericSum ys == weightNeeded i) $ subsequences (input \\ xs)
g2 xs = any (\ys -> genericSum ys == weightNeeded 4 && g ys 4) $ subsequences (input \\ xs)

genericSum = fromIntegral . sum

weightNeeded x = genericSum input / x

input :: [Integer]
input = map read $ words "1 3 5 11 13 17 19 23 29 31 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113"
