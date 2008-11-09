module P71 where

import Data.Ratio
import Data.List
import Euler

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : (merge xt ys)
    EQ -> x : (merge xt yt)
    GT -> y : (merge xs yt)

lists = map (\x -> [ a % x | a <- [1..x] ]) (reverse [1..1000000])

sortedNub [] = []
sortedNub [x] = [x]
sortedNub (x:y:xs) | x == y = sortedNub xs
                   | otherwise = x : (sortedNub (y:xs))


list = map (\n ->((3*n) `div` 7)%n) [1..1000000]

sorted = foldl1' merge lists

-- I need to generate these in order

