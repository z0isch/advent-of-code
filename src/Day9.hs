module Day9 where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.List
import           Text.Parsec

data CityLink = CityLink String String
  deriving (Show)

type Cities = HashMap String (HashMap String Int)

input :: String -> IO Cities
input s = parseInput <$> lines <$> readFile s

test = do
  i <- input "day9-test-input.txt"
  return $ map (\s -> routeFold minimum i (M.singleton s 0) s) (M.keys i)

partOne = do
  i <- input "day9-input.txt"
  return $ minimum $ map (\s -> routeFold minimum i (M.singleton s 0) s) (M.keys i)

partTwo = do
  i <- input "day9-input.txt"
  return $ maximum $ map (\s -> routeFold maximum i (M.singleton s 0) s) (M.keys i)

routeFold :: ([Int] -> Int) -> Cities -> HashMap String Int -> String -> Int
routeFold f cs chosen c
  | M.size cs == M.size chosen = M.foldl' (+) 0 chosen
  | otherwise = f $ map snd $ M.toList $ M.mapWithKey (\s i -> routeFold f cs (M.insert s i chosen) s) possibleCities
  where
    possibleCities :: HashMap String Int
    possibleCities = M.difference (cs M.! c) chosen

parseInput :: [String] -> Cities
parseInput = foldl citiesFold M.empty . map (either ( error . show) id  . parse cityLinkParser "")

citiesFold :: Cities -> (CityLink, Int) -> Cities
citiesFold c (CityLink s1 s2,w) = M.insertWith M.union s2 (M.singleton s1 w) $ M.insertWith M.union s1 (M.singleton s2 w) c

cityLinkParser = do
  e1 <- many1 letter
  string " to "
  e2 <- many1 letter
  string " = "
  w <- many1 digit
  return (CityLink e1 e2, read w)
