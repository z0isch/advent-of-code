module Day8 where

import           Text.Parsec

partOne = do
  i <- input
  return (totalCount codeLetterCount i - totalCount actualLetterCount i)
partTwo = do
  i <- input
  return (totalCount encodedLetterCount i - totalCount codeLetterCount i)

totalCount :: (String -> Int) -> [String] -> Int
totalCount f = sum . map f

encodedLetterCount :: String -> Int
encodedLetterCount = length . show
actualLetterCount :: String -> Int
actualLetterCount = flip (-) 2 . either ( error . show) id  . parse letterCountParser ""
codeLetterCount :: String -> Int
codeLetterCount = length

letterCountParser  = sum <$> many1 (try escapeParser <|> noEscapeParser)
noEscapeParser = pure 1 <$> anyChar
escapeParser = pure 1 <$> char '\\' <* (try hexParser <|> try slashParser <|> quoteParser)
hexParser = char 'x' <* hexDigit  <* hexDigit
slashParser = char '\\'
quoteParser = char '\"'

input :: IO [String]
input = lines <$> readFile "day8-input.txt"
