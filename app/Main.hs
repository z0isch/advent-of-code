module Main where

import           Codec.Picture
import qualified Data.ByteString.Lazy as L
import           Day18

main :: IO ()
main = do
  p1v <- visualizePartOne
  p2v <- visualizePartTwo
  bs <- either error return $ encodeGifAnimation 10 LoopingForever p1v
  L.writeFile "part-one.gif" bs
  bs <- either error return $ encodeGifAnimation 10 LoopingForever p2v
  L.writeFile "part-two.gif" bs
  -- p1 <- partOne
  -- print p1
  -- p2  <- partTwo
  -- print p2
  return ()
