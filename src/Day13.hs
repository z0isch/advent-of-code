{-# LANGUAGE FlexibleContexts #-}

module Day13 where

import           Control.Monad.Identity
import           Data.HashMap.Strict    (HashMap, (!))
import qualified Data.HashMap.Strict    as M
import           Data.List
import           Text.Parsec

type TableMap = HashMap String (HashMap String Int)

partOne = do
  i <- input
  let m = foldl (\hm a -> either ( error . show) id  $ parse (lineParser hm) "" a) M.empty i
  let tables = permutations $ M.keys m
  return $ maximum $ map (tableHappiness m) tables

partTwo = do
  i <- input
  let m = foldl (\hm a -> either ( error . show) id  $ parse (lineParser hm) "" a) M.empty i
      m2 = M.map (M.insert "Me" 0) m
      mWithMe = M.insert "Me" (M.fromList (map (\k -> (k,0)) (M.keys m))) m2
      tables = permutations $ M.keys mWithMe
  return $ maximum $ map (tableHappiness mWithMe) tables

tableHappiness :: TableMap -> [String] -> Int
tableHappiness tm t = sum $ map adjecencyHappiness adjacencies
  where
    adjacencies = [head t, last t] : take (length t - 1) (map (take 2) (tails t))
    adjecencyHappiness [a1,a2] =  (tm ! a1) ! a2 + (tm ! a2) ! a1

input :: IO [String]
input = lines <$> readFile "day13-input.txt"

lineParser :: TableMap -> ParsecT String u Identity TableMap
lineParser hm = do
  p1 <- many1 letter
  string  " would "
  s <- try (pure 1 <$> string "gain ") <|> (pure (-1) <$> string "lose ")
  w <- (*) s <$> read <$> many1 digit
  string " happiness units by sitting next to "
  p2 <- many1 letter <* char '.'
  return $ M.insertWith M.union p1 (M.singleton p2 w) hm
