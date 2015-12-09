module  Day7 where

import           Control.Monad.Identity (Identity)
import           Data.Bits
import           Data.Function          (fix)
import           Data.Function.Memoize
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as M
import           Data.List
import qualified Data.Map.Lazy          as LM
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
  return $ eval m "a"
partTwo = do
  m <- input
  let val1 = eval m "a"
  let m2 = M.adjust (const (Const (Value val1))) "b" m
  return $ eval m2 "a"

evalList :: HashMap String WireInput -> LM.Map WireInput CUShort
evalList m = LM.fromList $ map (\w -> (w,eval' fasterEval m w)) wInputList
  where wInputList = nub $ map (Const . Var) (M.keys m) ++ M.elems m

fasterEval :: HashMap String WireInput -> WireInput -> CUShort
fasterEval m w =  evalList m LM.! w

eval' :: (HashMap String WireInput -> WireInput -> CUShort) -> HashMap String WireInput -> WireInput -> CUShort
eval' f m (And x y) = getAtom' f m x .&. getAtom' f m y
eval' f m (Const x) = getAtom' f m x
eval' f m (Not x) = complement (getAtom' f m x)
eval' f m (Or x y) = getAtom' f m x  .|. getAtom' f m y
eval' f m (RShift x y) = getAtom' f m x `shiftR` fromIntegral (getAtom' f m y)
eval' f m (LShift x y) = getAtom' f m x `shiftL` fromIntegral (getAtom' f m y)

getAtom' :: (HashMap String WireInput -> WireInput -> CUShort) -> HashMap String WireInput -> Atom -> CUShort
getAtom' _ _ (Value i) = i
getAtom' f m (Var s) = f m (m M.! s)

eval :: HashMap String WireInput -> String -> CUShort
eval m = me
  where
    e :: String -> CUShort
    e s = case m M.! s of
      (Const x) -> getAtom x
      (And x y) -> getAtom x .&. getAtom y
      (Or x y) -> getAtom x  .|. getAtom y
      (Not x) -> complement (getAtom x)
      (RShift x y) -> getAtom x `shiftR` fromIntegral (getAtom y)
      (LShift x y) -> getAtom x `shiftL` fromIntegral (getAtom y)
    me :: String -> CUShort
    me = memoize e
    getAtom :: Atom -> CUShort
    getAtom (Value i) = i
    getAtom (Var s) = me s

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
