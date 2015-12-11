module Day11 where

import           Control.Arrow
import           Data.Function
import           Data.List
import           Data.Maybe

partOne = nextGoodPass input
partTwo = nextGoodPass partOne

nextGoodPass :: String -> String
nextGoodPass s = fromJust $ find goodPass $ filter (/= s) $ iterate nextWord s

nextWord :: String -> String
nextWord = reverse . revNextWord . reverse

revNextWord :: String -> String
revNextWord (x:xs)
  | x < 'z' = nextChar x : xs
  | otherwise = 'a' : revNextWord xs

nextChar :: Char -> Char
nextChar c = head $ dropWhile (c >=) ['a'..'z']

goodPass :: String -> Bool
goodPass s = stringOfThree s && noLetters "iol" s && twoNonOverlappingPairs s

stringOfThree :: String -> Bool
stringOfThree = any (`isInfixOf` ['a'..'z']) . filter ((==) 3 . length) . map (take 3) . tails

noLetters :: String -> String -> Bool
noLetters s = all (`notElem` s)

twoNonOverlappingPairs :: String -> Bool
twoNonOverlappingPairs = (> 1) . length . nubBy ((==) `on` fst) . filter (\cs -> snd cs > 1) . map (head &&& length) . group

input :: String
input = "hxbxwxba"
