{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import           Data.Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL
import qualified Data.HashMap.Strict   as M
import           Data.Maybe
import           Data.Scientific
import           Data.Text
import qualified Data.Vector           as V

partOne = numberFold (const True) 0 <$> input
partTwo = numberFold (notElem "red" . M.elems) 0 <$> input

numberFold :: (M.HashMap Text Value -> Bool) -> Scientific -> Value -> Scientific
numberFold f s (Object o) = if f o then M.foldl' (numberFold f) s o else s
numberFold f s (Array a) = V.foldl (numberFold f) s a
numberFold _ s (Number b) = s + b
numberFold _ s _ = s

input :: IO Value
input = fromJust <$> decode <$> BL.fromStrict <$> BC.readFile "day11-input.txt"
