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
  deriving (Show, Eq)
data WireInput = Const Atom
  | And Atom Atom
  | LShift Atom Atom
  | RShift Atom Atom
  | Not Atom
  | Or Atom Atom
  deriving (Show, Eq)

partOne = do
  m <- input
  return $ getAtom m (Var "a")

evalList :: HashMap String WireInput -> [CUShort]
evalList m = map (eval'' fasterEval m . Const . Var) (M.keys m)

fasterEval :: HashMap String WireInput -> WireInput -> CUShort
fasterEval m w = undefined --evalList m !! 0

eval'' :: (HashMap String WireInput -> WireInput -> CUShort) -> HashMap String WireInput -> WireInput -> CUShort
eval'' f m (Const (Var i)) = f m (unsafeLookup m i)
eval'' f m (Const (Value i)) = i
eval'' f m (And (Value i) (Var x)) = i .&. f m (unsafeLookup m x)
eval'' f m (And (Var i) (Var x)) = f m (unsafeLookup m i) .&. f m (unsafeLookup m x)
eval'' f m (And (Var i) (Value x)) = f m (unsafeLookup m i) .&. x
eval'' f m (And (Value i) (Value x)) = i .&. x
eval'' f m (Or (Value i) (Var x)) = i .|. f m (unsafeLookup m x)
eval'' f m (Or (Var i) (Var x)) = f m (unsafeLookup m i) .|. f m (unsafeLookup m x)
eval'' f m (Or (Var i) (Value x)) = f m (unsafeLookup m i) .|. x
eval'' f m (Or (Value i) (Value x)) = i .|. x
eval'' f m (Not (Value i)) = complement i
eval'' f m (Not (Var i)) = complement (f m (unsafeLookup m i))
eval'' f m (RShift (Value i) (Var x)) = i `shiftR` fromIntegral (f m (unsafeLookup m x))
eval'' f m (RShift (Var i) (Var x)) = f m (unsafeLookup m i) `shiftR` fromIntegral (f m (unsafeLookup m x))
eval'' f m (RShift (Var i) (Value x)) = f m (unsafeLookup m i) `shiftR` fromIntegral x
eval'' f m (RShift (Value i) (Value x)) = i `shiftR` fromIntegral x
eval'' f m (LShift (Value i) (Var x)) = i `shiftL` fromIntegral (f m (unsafeLookup m x))
eval'' f m (LShift (Var i) (Var x)) = f m (unsafeLookup m i) `shiftL` fromIntegral (f m (unsafeLookup m x))
eval'' f m (LShift (Var i) (Value x)) = f m (unsafeLookup m i) `shiftL` fromIntegral x
eval'' f m (LShift (Value i) (Value x)) = i `shiftL` fromIntegral  x

eval' m (Const (Var i)) = eval' m (unsafeLookup m i)
eval' m (Const (Value i)) = i
eval' m (And (Value i) (Var x)) = i .&. eval' m (unsafeLookup m x)
eval' m (And (Var i) (Var x)) = eval' m (unsafeLookup m i) .&. eval' m (unsafeLookup m x)
eval' m (And (Var i) (Value x)) = eval' m (unsafeLookup m i) .&. x
eval' m (And (Value i) (Value x)) = i .&. x
eval' m (Or (Value i) (Var x)) = i .|. eval' m (unsafeLookup m x)
eval' m (Or (Var i) (Var x)) = eval' m (unsafeLookup m i) .|. eval' m (unsafeLookup m x)
eval' m (Or (Var i) (Value x)) = eval' m (unsafeLookup m i) .|. x
eval' m (Or (Value i) (Value x)) = i .|. x
eval' m (Not (Value i)) = complement i
eval' m (Not (Var i)) = complement (eval' m (unsafeLookup m i))
eval' m (RShift (Value i) (Var x)) = i `shiftR` fromIntegral (eval' m (unsafeLookup m x))
eval' m (RShift (Var i) (Var x)) = eval' m (unsafeLookup m i) `shiftR` fromIntegral (eval' m (unsafeLookup m x))
eval' m (RShift (Var i) (Value x)) = eval' m (unsafeLookup m i) `shiftR` fromIntegral x
eval' m (RShift (Value i) (Value x)) = i `shiftR` fromIntegral x
eval' m (LShift (Value i) (Var x)) = i `shiftL` fromIntegral (eval' m (unsafeLookup m x))
eval' m (LShift (Var i) (Var x)) = eval' m (unsafeLookup m i) `shiftL` fromIntegral (eval' m (unsafeLookup m x))
eval' m (LShift (Var i) (Value x)) = eval' m (unsafeLookup m i) `shiftL` fromIntegral x
eval' m (LShift (Value i) (Value x)) = i `shiftL` fromIntegral  x

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
