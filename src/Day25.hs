module Day25 where

import           Data.List
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V

partOne = gen $ k (3018,3009) - 1

k (x,y) = g (g 1 0 y) (y+1) x

gen 0 = 20151125
gen i = (252533 * gen (i-1)) `mod` 33554393

g x _ 0 = x
g x y i = (y+i) + g x y (i-1)
