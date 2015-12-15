module Day14 where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.List

data FlightStats = FlightStats String [(Double,Double)]
  deriving (Show)

partOne = maximum <$> map (distanceFlown 2503) <$> input

partTwo = do
  fs <- input
  return $ maximumBy (\(_,s1) (_,s2) -> compare s1 s2) $ histogram (winners fs 1 2503)

raceStats :: [FlightStats] -> Double -> Double -> [[(String, Double)]]
raceStats fs start end = map (\s -> map (distanceFlown s) fs) [start..end]

winners :: [FlightStats] -> Double -> Double -> [String]
winners fs s e = concatMap first (raceStats fs s e)

first :: [(String,Double)] -> [String]
first stats = map fst $ takeWhile (\(_,s) -> s == snd (head (sorted stats))) $ sorted stats
  where sorted = reverse . sortOn snd

histogram :: Ord a => [a] -> [(a,Int)]
histogram xs = [ (head l, length l) | l <- group (sort xs) ]

distanceFlown :: Double -> FlightStats -> (String,Double)
distanceFlown t (FlightStats s xs) = (,) s $ fst $ foldl calcDist (0,0) fs
  where
    calcDist :: (Double,Double) -> (Double,Double) -> (Double, Double)
    calcDist (d,tt) (s,rt)
      | tt == t = (d,t)
      | (tt + rt) <= t = (d + (s*rt), tt+rt)
      | otherwise = (dist, t)
        where dist = d + (s * rt * ((t-tt) / rt))
    fs :: [(Double,Double)]
    fs = take numNeeded $ cycle xs
    numNeeded :: Int
    numNeeded =  length xs * fromIntegral (ceiling $ t / sum (map snd xs))

input :: IO [FlightStats]
input = map parse <$> lines <$> readFile "day14-input.txt"

parse :: String -> FlightStats
parse s = go $ words s
  where
    go [r,_,_,s1,_,_,t1,_,_,_,_,_,_,t2,_] = FlightStats r [(read s1,read t1),(0,read t2)]

test = map (distanceFlown 1000) testInput
test2 = histogram (winners testInput 1 1000)
testInput = map parse $ lines "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.\nDancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
