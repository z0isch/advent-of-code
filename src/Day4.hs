module Day4
where

import Crypto.Hash.MD5
import Data.ByteString.Char8 (pack)
import Data.Hex
import qualified Data.ByteString as B
import  Data.ByteString(ByteString)

partTwo :: Int
partTwo = solve (pack "000000")

partOne :: Int
partOne = solve (pack "00000")

solve :: ByteString -> Int
solve c = (+) 1 $ length $
  takeWhile (not . B.isPrefixOf c) $
  map (getHash . (input ++) . show) [1..]

getHash :: String -> ByteString
getHash = hex . hash . pack

input :: String
input = "ckczppom"
