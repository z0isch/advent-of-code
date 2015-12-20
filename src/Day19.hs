module Day19 where

import           Control.Parallel.Strategies
import           Data.List
import           Data.Vector                 (Vector, (!), (//))
import qualified Data.Vector                 as V
import           Text.Parsec

type Transform = (String,String)
type Medicine = String

partOne = do
  (m,trs) <- input
  return $ length $ genTransforms trs m

partTwo = do
  (m,trs) <- input
  return $ (+) 1 $ fst $ last $ takeWhileInclusive (\(_,ms) -> notElem m ms) $ iterate (step trs) (0,start trs)

test2 = (+ 1) $ fst $ last $ takeWhileInclusive (\(_,ms) -> notElem "HOHOHO" ms) $ iterate (step trs) (0,start trs)
  where trs = [("e","H"),("e","O"),("H","HO"),("H","OH"),("O","HH")]

step :: [Transform] -> (Int,[Medicine]) -> (Int,[Medicine])
step trs (i,ms) = (i+1, nub (concat $ parMap rseq (genTransforms trs) ms))

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []
start :: [Transform] -> [Medicine]
start  = map snd . filter (\(i,o) -> i == "e")

genTransforms :: [Transform] -> Medicine -> [Medicine]
genTransforms trs m = nub $ concat $ zipWith (\tr is -> map (applyTransform mVec) is) trs transforms
  where
    mVec = V.fromList m
    transforms = map (findTransforms m 0) trs

applyTransform :: Vector Char -> (String,(Int,String)) -> String
applyTransform m (i,(index,o)) = V.toList $ V.take index m V.++ V.fromList o V.++ V.drop (index+length i) m

findTransforms :: Medicine -> Int -> Transform -> [(String,(Int,String))]
findTransforms [] _ _ = []
findTransforms m@(x:xs) i tr@(t,o)
  | t `isPrefixOf` m = (t,(i,o)) : findTransforms (drop (length t) m) (i + length t) tr
  | otherwise = findTransforms xs (i+1) tr

input :: IO (Medicine,[Transform])
input = either ( error . show) id  . parse inputParser "" <$> readFile "day19-input.txt"

transformParser = do
  input <- many1 letter
  _ <- string " => "
  output <- many1 letter
  _ <- endOfLine
  return (input,output)

medicineParser = many1 letter <* endOfLine

inputParser = do
  ts <- many1 transformParser
  _ <- endOfLine
  m <- medicineParser
  return (m,ts)
