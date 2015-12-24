module Day23 where

import           Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as H
import           Data.List
import           Text.Parsec

data Instruction = Hlf String | Tpl String | Inc String | Jmp Integer | Jie String Integer | Jio String Integer
  deriving (Show)

test = eval testInput (head testInput) (0,H.fromList [("a",1),("b",0)])
testInput= either ( error . show) id  $ parse instructionsParser  "" "inc a\njio a, +2\ntpl a\ninc a\n"

partOne = do
  is <- input
  return $ eval is (head is) (0,H.fromList [("a",0),("b",0)])

partTwo = do
  is <- input
  return $ eval is (head is) (0,H.fromList [("a",1),("b",0)])

eval :: [Instruction] -> Instruction -> (Integer,HashMap String Integer) -> (Integer,HashMap String Integer)
eval is (Hlf a) s   = evalFunc is a s (`quot` 2)
eval is (Tpl a) s   = evalFunc is a s (* 3)
eval is (Inc a) s   = evalFunc is a s (+ 1)
eval is (Jmp o) s   = evalJump is "" o s (const True)
eval is (Jie a o) s = evalJump is a o s even
eval is (Jio a o) s = evalJump is a o s (== 1)

evalFunc is a (i,hm) f
  | (i+1) < genericLength is = eval is (is !! fromIntegral (i+1)) (i+1,H.adjust f a hm)
  | otherwise = (i,H.adjust f a hm)
evalJump is a o (i,hm) f
  | f (hm ! a)       = if (i+o) < fromIntegral (length is) && (i+o) > -1
                       then eval is (is !! fromIntegral (i+o)) (i+o,hm)
                       else (i,hm)
  | not $ f (hm ! a) = if (i+1) < genericLength is
                       then eval is (is !! fromIntegral (i+1)) (i+1,hm)
                       else (i,hm)


input :: IO [Instruction]
input = either ( error . show) id  . parse instructionsParser  "" <$> readFile "day23-input.txt"

instructionsParser = many1 instructionParser
instructionParser = try hlfParser <|> try tplParser <|> try incParser <|> try jmpParser <|> try jieParser <|> jioParser

offsetParser = try posParser <|> negParser
negParser = do
  _ <- char '-'
  r <- read <$> many1 digit
  return $ (-1) * r
posParser = do
  _ <- char '+'
  read <$> many1 digit
jmpParser = do
  string "jmp "
  o <- offsetParser
  endOfLine
  return $ Jmp o
jioParser = do
  string "jio "
  r <- many1 letter
  string ", "
  o <- offsetParser
  endOfLine
  return $ Jio r o
jieParser = do
  string "jie "
  r <- many1 letter
  string ", "
  o <- offsetParser
  endOfLine
  return $ Jie r o
incParser = do
  string "inc "
  r <- many1 letter
  endOfLine
  return $ Inc r
hlfParser = do
  string "hlf "
  r <- many1 letter
  endOfLine
  return $ Hlf r
tplParser = do
  string "tpl "
  r <- many1 letter
  endOfLine
  return $ Tpl r
