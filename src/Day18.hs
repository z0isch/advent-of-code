module Day18 where

import           Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import           Text.Parsec

type Grid = Vector (Vector Int)

partOne = do
  g <- input
  let gOut = last $ take 101 $ iterate (step toggle1) g
  return $ lightsOn gOut

partTwo = do
  g <- input
  let gOut = last $ take 101 $ iterate (step toggle2) (setUpPart2 g)
  return $ lightsOn gOut

test2 = do
  g <- testInput
  let gOut = last $ take 6 $ iterate (step toggle2) (setUpPart2 g)
  return $ lightsOn gOut
test1 = do
  g <- testInput
  let gOut = last $ take 5 $ iterate (step toggle1) g
  return $ lightsOn gOut

setUpPart2 :: Grid -> Grid
setUpPart2 g = g //
  [ (0, setCoords 0)
  , (maxCoord, setCoords maxCoord)]
  where
    setCoords c = g ! c // [(0,1),(maxCoord,1)]
    maxCoord = V.length g - 1

lightsOn :: Grid -> Int
lightsOn = V.sum . V.map V.sum

step :: (Grid -> (Int,Int) -> Int -> [Int] -> Int) -> Grid -> Grid
step toggle g = V.imap (\x -> V.imap (\y l -> toggle g (x,y) l (neighbors g (x,y)))) g

toggle1 :: Grid -> (Int,Int) -> Int -> [Int] -> Int
toggle1 _ _ s ns
  | s == 1 = if on == 2 || on == 3 then 1 else 0
  | s == 0 = if on == 3 then 1 else 0
  where
  on = sum ns

toggle2 :: Grid -> (Int,Int) -> Int -> [Int] -> Int
toggle2 g (x,y) s ns
  | x == 0 && y == 0 = 1
  | x == maxCoord && y == 0 = 1
  | x == 0 && y == maxCoord = 1
  | x == maxCoord && y == maxCoord = 1
  | s == 1 = if on == 2 || on == 3 then 1 else 0
  | s == 0 = if on == 3 then 1 else 0
  where
  on = sum ns
  maxCoord = V.length g - 1

neighbors :: Grid -> (Int,Int) -> [Int]
neighbors n (x,y) = map (\(nx,ny) -> n ! nx ! ny) ps
  where
    maxSize = V.length n
    xs = [x-1..x+1]
    ys = [y-1..y+1]
    ps = [(nx,ny) | nx <- xs, ny <- ys, nx >= 0 && nx < maxSize, ny >= 0 && ny < maxSize, nx /= x || ny /= y]

lightParser = pure 0 <* try (char '.') <|> pure 1 <* char '#'
lineParser = many1 lightParser

input :: IO Grid
input = readInput "day18-input.txt"
testInput :: IO Grid
testInput = readInput "day18-test-input.txt"
readInput f = V.fromList <$> map (V.fromList . either ( error . show) id  . parse lineParser "") <$> lines <$> readFile f
