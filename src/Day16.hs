module Day16 where

import           Control.Lens
import           Control.Monad.Identity
import           Data.List
import           Data.Maybe
import           Text.Parsec

partOne = do
  i <- input
  return $ last $ sortOn snd $ map (over _2 (score scoreVal sueIn)) i

partTwo = do
  i <- input
  return $ last $ sortOn snd $ map (over _2 (score scoreVal2 sueIn)) i

score f sI = foldl (\n (d,num) -> n + maybe 0 (f d num) (lookup d sI)) 0
scoreVal d num n = if num == n then 1 else 0
scoreVal2 d num n
  | d == "cats" || d == "trees"  = if num > n then 1 else 0
  | d == "pomeranians" || d == "goldfish" = if num < n then 1 else 0
  | otherwise = if num == n then 1 else 0

parseInput :: String -> (Int,[(String,Int)])
parseInput = either ( error . show) id  . parse sueParser ""
input :: IO [(Int,[(String,Int)])]
input = map parseInput <$> lines <$> readFile "day16-input.txt"

sueParser :: ParsecT String u Identity (Int,[(String,Int)])
sueParser = do
  i <- sueIDParser
  ds <- many1 sueDataParser
  return (i,ds)

sueIDParser :: ParsecT String u Identity Int
sueIDParser = do
  _ <- string "Sue "
  i <- read <$> many1 digit
  _ <- string ": "
  return i

sueDataParser :: ParsecT String u Identity (String,Int)
sueDataParser = do
  s <- many1 letter
  _ <- string ": "
  d <- read <$> many1 digit
  _ <- optional $ string ", "
  return (s,d)

sueIn :: [(String,Int)]
sueIn =
  [ ("children",3)
  , ("cats",7)
  , ("samoyeds",2)
  , ("pomeranians",3)
  , ("akitas",0)
  , ("vizslas",0)
  , ("goldfish",5)
  , ("trees",3)
  , ("cars",2)
  , ("perfumes",1)
  ]
