module Day20 where

import           Control.Parallel.Strategies
import           Data.List
import           Data.Numbers.Primes

partOne = last $ takeWhileInclusive (\(h,p) -> p <= input) $ map presents [1..]
partTwo = last $ takeWhileInclusive (\(h,p) -> p <= input) $ map presents2 [1..]

presents :: Int -> (Int,Int)
presents h = (h, sum $ map (* 10) $ factors' h)

presents2 :: Int -> (Int,Int)
presents2 h = (h, sum $ map (* 11) fs)
  where
    fs = filter (\f -> (fromIntegral h / fromIntegral f) <= 50) $ factors' h

factors' :: Int -> [Int]
factors' x = nub $ map product $ subsequences $ primeFactors x

factors :: Int -> [Int]
factors x = map fst $ filter (\(t,z) -> z == 0) $ parMap rdeepseq (\t -> (t,mod x t)) [1..x]

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []
                                         
input = 34000000
