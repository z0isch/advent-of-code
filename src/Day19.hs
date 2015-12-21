module Day19 where

import           Control.Parallel.Strategies
import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as HS
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
  let revTrs = map (\(x,y) -> (y,x)) (nonE trs)
  return $ takeWhileInclusive (== -1) $ transformTree revTrs (end trs) [m] 1

test2 = transformTree revTrs (end trs) ["HOHOHO"] 0
  where
    revTrs = map (\(x,y) -> (y,x)) (nonE trs)
    trs = [("e","H"),("e","O"),("H","HO"),("H","OH"),("O","HH")]

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []

transformTree :: [Transform] -> [Medicine] -> [Medicine] -> Int -> [Int]
transformTree trs bs [] i = [-1]
transformTree trs bs (m:ms) i
  | null newMeds = [-1]
  | any (`HS.member` newMeds) bs = [i+1]
  | otherwise = -1 : transformTree trs bs (HS.toList newMeds) (i+1) ++  transformTree trs bs ms (i+1)
  where
    newMeds = genTransforms trs m

nonE = filter (\(x,y) -> x /= "e")
end = map snd . filter (\(x,y) -> x == "e")

start :: [Transform] -> [Medicine]
start  = map snd . filter (\(i,o) -> i == "e")

genTransforms :: [Transform] -> Medicine -> HashSet Medicine
genTransforms trs m = HS.fromList $ concat $ zipWith (\tr is -> parMap rdeepseq (applyTransform mVec) is) trs transforms
  where
    mVec = V.fromList m
    transforms = parMap rdeepseq (findTransforms m 0) trs

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
