module  Day7 where

import           Control.Monad.Identity (Identity)
import           Data.Bits
import           Data.Function          (fix)
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as M
import           Data.List
import           Data.Maybe
import           Foreign.C.Types
import           Text.Parsec

data Atom = Value CUShort | Var String
  deriving (Show, Eq, Ord)
data WireInput = Const Atom
  | And Atom Atom
  | LShift Atom Atom
  | RShift Atom Atom
  | Not Atom
  | Or Atom Atom
  deriving (Show, Eq, Ord)

partOne = do
  m <- input
  return $ fasterEval m (Const (Var "a"))

evalList :: HashMap String WireInput -> [(WireInput,CUShort)]
evalList m = map (\w -> (w,eval' fasterEval m w)) wInputList
  where wInputList = M.elems m ++ map (Const . Var) (M.keys m)

fasterEval :: HashMap String WireInput -> WireInput -> CUShort
fasterEval m w = fromJust (lookup w $ evalList m)

eval' :: (HashMap String WireInput -> WireInput -> CUShort) -> HashMap String WireInput -> WireInput -> CUShort
eval' f m (And x y) = getAtom' f m x .&. getAtom m y
eval' f m (Const x) = getAtom' f m x
eval' f m (Not x) = complement (getAtom' f m x)
eval' f m (Or x y) = getAtom' f m y  .|. getAtom' f m y
eval' f m (RShift x y) = getAtom' f m x `shiftR` fromIntegral (getAtom' f m y)
eval' f m (LShift x y) = getAtom' f m x `shiftL` fromIntegral (getAtom' f m y)

getAtom' :: (HashMap String WireInput -> WireInput -> CUShort) -> HashMap String WireInput -> Atom -> CUShort
getAtom' _ _ (Value i) = i
getAtom' f m (Var s) = f m (unsafeLookup m s)

eval :: (Atom -> CUShort) -> WireInput -> CUShort
eval f (Const x) = f x
eval f (And x y) = f x .&. f y
eval f (Not x) = complement (f x)
eval f (Or x y) = f y  .|. f y
eval f (RShift x y) = fromIntegral (f x) `shiftR` fromIntegral (f y)
eval f (LShift x y) = fromIntegral (f x) `shiftL` fromIntegral (f y)

getAtom :: HashMap String WireInput -> Atom -> CUShort
getAtom _ (Value i) = i
getAtom m (Var s) = eval (getAtom m) (unsafeLookup m s)

unsafeLookup m x = fromJust $ M.lookup x m

parseInput :: [String] -> [(String,WireInput)]
parseInput = map (either ( error . show) id  . parse wireInputParser "")

wireInputParser = flip (,) <$> parseNode <* string " -> " <*> many1 letter
parseNode = try parseNot
  <|> try parseAnd
  <|> try parseOr
  <|> try parseLShift
  <|> try parseRShift
  <|> parseConst
parseAtom = try parseValue <|> parseVar
parseValue = Value . read <$> many1 digit
parseVar = Var <$> many1 letter
parseConst = Const <$> parseAtom
parseNot = Not <$> (string "NOT " *> parseAtom)
parseAnd = And <$> parseAtom <* string " AND " <*> parseAtom
parseOr = Or <$> parseAtom <* string " OR " <*> parseAtom
parseLShift = LShift <$> parseAtom <* string " LSHIFT " <*> parseAtom
parseRShift = RShift <$> parseAtom <* string " RSHIFT " <*> parseAtom

input :: IO (HashMap String WireInput)
input = M.fromList <$> parseInput <$> lines <$> readFile "day7-input.txt"
