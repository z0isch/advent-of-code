module Main where

import           Codec.Picture
import           Codec.Picture.Types
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector          as V
import           Day19
import           Graphics.Rasterific

main :: IO ()
main = do
  -- p1v <- visualizePartOne
  -- p2v <- visualizePartTwo
  -- bs <- either error return $ encodeGifAnimation 10 LoopingForever (sizeImages 250 250 p1v)
  -- L.writeFile "part-one.gif" bs
  -- bs <- either error return $ encodeGifAnimation 10 LoopingForever (sizeImages 250 250 p2v)
  -- L.writeFile "part-two.gif" bs
  p1 <- partOne
  print p1
  p2  <- partTwo
  print p2
  return ()

sizeImages :: Int -> Int -> [Image PixelRGB8] -> [Image PixelRGB8]
sizeImages h w = map (\i -> pixelMap dropTransparency $ renderDrawing h w (PixelRGBA8 255 255 255 255) $ drawImageAtSize (promoteImage i) 0 (V2 0 0) (fromIntegral h) (fromIntegral w))
