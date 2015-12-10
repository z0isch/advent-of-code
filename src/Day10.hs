module Day10 where

import           Data.List

partOne = length $ iterate speakAndSay input !! 40
partTwo = length $ iterate speakAndSay input !! 50

speakAndSay :: String -> String
speakAndSay =  concatMap (\x -> show (length x) ++ [head x]) . group

input :: String
input = "1321131112"

testInput = "1"
