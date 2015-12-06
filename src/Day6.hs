module Day6 where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Identity (Identity)
import           Data.Bits
import           Text.Parsec

data Instruction = Instruction InstructionType (Int,Int) (Int,Int)
  deriving (Show)
data InstructionType = Toggle | On | Off
  deriving (Show)
type Grid = [(Int,[(Int,Int)])]

partOne :: IO Int
partOne = do
  is <- input
  let result = foldl applyInstruction startGrid is
  return $ length $ concatMap (\(i,xs) -> filter (\(_,x) -> x >= 1) xs) result

partTwo :: IO Int
partTwo = do
  is <- input
  let result = foldl applyInstruction2 startGrid is
  return $ sum $ concatMap (\(i,xs) -> map snd $ filter (\(_,x) -> x >= 1) xs) result

gridSize :: Int
gridSize = 1000

applyInstruction2 :: Grid -> Instruction -> Grid
applyInstruction2 g (Instruction On a1 a2) = applyY (1 +) a1 a2 g
applyInstruction2 g (Instruction Off a1 a2) = applyY (\i -> if i > 0 then i - 1 else i) a1 a2 g
applyInstruction2 g (Instruction Toggle a1 a2) = applyY (2 +) a1 a2 g

applyInstruction :: Grid -> Instruction -> Grid
applyInstruction g (Instruction On a1 a2) = applyY (const 1) a1 a2 g
applyInstruction g (Instruction Off a1 a2) = applyY (const 0) a1 a2 g
applyInstruction g (Instruction Toggle a1 a2) = applyY (\i -> if i == 1 then 0 else 1) a1 a2 g

applyY :: (Int -> Int) -> (Int,Int) -> (Int,Int) -> Grid -> Grid
applyY f (x1,y1) (x2,y2) = map (\(i,s) -> if i >= y1 && i <= y2 then (i, applyX f x1 x2 s) else (i,s))
applyX :: (Int -> Int) -> Int -> Int -> [(Int,Int)] -> [(Int,Int)]
applyX f x1 x2 = map (\(i,s) -> if i >= x1 && i <= x2 then (i,f s) else (i,s))

startGrid :: Grid
startGrid = zip [0..] (replicate gridSize $ zip [0..] (replicate gridSize 0))

parseInput :: [String] -> [Instruction]
parseInput = map (either ( error . show) id  . parse instructionParser "")

instructionParser :: ParsecT String u Identity Instruction
instructionParser = do
  t <- instructionTypeParser
  _ <- char ' '
  a1 <- coordParser
  _ <- string " through "
  a2 <- coordParser
  return $ Instruction t a1 a2

instructionTypeParser :: ParsecT String u Identity InstructionType
instructionTypeParser = try (Toggle <$ string "toggle")
  <|> try (On <$ string "turn on")
  <|> try (Off <$ string "turn off")

coordParser :: ParsecT String u Identity (Int,Int)
coordParser = do
  x <- many1 digit
  _ <- char ','
  y <- many1 digit
  return (read x, read y)

input :: IO [Instruction]
input = parseInput <$> lines <$> readFile "day6-input.txt"
