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
  return $ transformTree revTrs (end trs) [m] maxBound 1

test2 = transformTree revTrs (end trs) ["HOHOHO"] maxBound 1
  where
    revTrs = map (\(x,y) -> (y,x)) (nonE trs)
    trs = [("e","H"),("e","O"),("H","HO"),("H","OH"),("O","HH")]

transformTree :: [Transform] -> [Medicine] -> [Medicine] -> Int -> Int -> [Int]
transformTree _ _ [] minDepth _ = [minDepth]
transformTree trs bs (m:ms) minDepth i
  | (i+1) >= minDepth = [minDepth]
  | any (`HS.member` newMeds) bs = [i + 1]
  | otherwise = newMin : transformTree trs bs ms newMin i
  where
    newMeds = genTransforms trs m
    b = transformTree trs bs (HS.toList newMeds) minDepth (i+1)
    minB = if null b then maxBound else minimum b
    newMin = if minB < minDepth then minB else minDepth

nonE = filter (\(x,y) -> x /= "e")
end = map snd . filter (\(x,y) -> x == "e")

genTransforms :: [Transform] -> Medicine -> HashSet Medicine
genTransforms trs m = HS.fromList $ concat $ zipWith (\is tr -> map (applyTransform mVec) is) transforms trs
  where
    mVec = V.fromList m
    transforms = parMap rdeepseq (findTransforms m 0) trs

applyTransform :: Vector Char -> (String,(Int,String)) -> String
applyTransform m (i,(index,o)) = V.toList $ V.take index m V.++ V.fromList o V.++ V.drop (index+length i) m

findTransforms :: Medicine -> Int -> Transform -> [(String,(Int,String))]
findTransforms [] _ _ = []
findTransforms m@(x:xs) i tr@(t,o)
  | t `isPrefixOf` m =  (t,(i,o)) : findTransforms (drop (length t) m) (i + length t) tr
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
