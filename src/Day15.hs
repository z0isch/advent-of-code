{-# LANGUAGE TemplateHaskell #-}

module Day15 where

import           Control.Lens
import           Control.Monad
import           Data.List
import           Linear.Matrix
import           Linear.V4

data Ingredient = Ingredient { _capacity :: Int, _durability :: Int, _flavor :: Int, _texture :: Int, _calories :: Int}
  deriving (Show)
makeLenses ''Ingredient

partOne = last $ sortOn snd $
  map (over _2 score . (\v -> (v,calcSweets v))) $
  combinations 100
partTwo = last $ sortOn snd $
  map (over _2 score) $
  filter (\(v,_) -> calorieCount v == 500)  $
  map (\v -> (v,calcSweets v)) $
  combinations 100

calcSweets :: V4 Int -> V4 Int
calcSweets v = sweets input !* v

combinations :: Int -> [V4 Int]
combinations m = do
  n1 <- [0..m]
  n2 <- [0..m]
  n3 <- [0..m]
  n4 <- [0..m]
  guard $ n1 + n2 + n3 + n4 == m
  return $ V4 n1 n2 n3 n4

calorieCount :: V4 Int -> Int
calorieCount (V4 w x y z) = (w * c 0) + (x * c 1) + (y * c 2) + (z * c 3)
  where c i = (input !! i)^.calories

score :: V4 Int -> Int
score (V4 c d f t)
  | (c<0) || (d<0) || (f<0) || (t<0) = 0
  | otherwise = c*d*f*t

sweets :: [Ingredient] -> V4 (V4 Int)
sweets is = V4
  (uniqueConst $ map _capacity is)
  (uniqueConst $ map _durability is)
  (uniqueConst $ map _flavor is)
  (uniqueConst $ map _texture is)

uniqueConst :: [Int] -> V4 Int
uniqueConst [c1,c2,c3,c4] = V4 c1 c2 c3 c4

parseInput :: String -> Ingredient
parseInput  =  go . words
  where
    go [s,_,c,_,d,_,f,_,t,_,cal] = Ingredient (p c) (p d) (p f) (p t) (p cal)
    p :: String -> Int
    p s = read $ takeWhile (',' /=) s

input = map parseInput $ lines "Sprinkles: capacity 2, durability 0, flavor -2, texture 0, calories 3\nButterscotch: capacity 0, durability 5, flavor -3, texture 0, calories 3\nChocolate: capacity 0, durability 0, flavor 5, texture -1, calories 8\nCandy: capacity 0, durability -1, flavor 0, texture 5, calories 8"
