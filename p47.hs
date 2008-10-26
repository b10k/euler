module P47 where

import Euler
import ONeillPrimes
import Data.List (nub)

pfactors = filter (\n -> (length . nub . factor) n == 4) [1..]

fourConsec (x1:x2:x3:x4:xs) | x1+1 == x2 && x2 + 1 == x3 && x3 + 1 == x4 = True
                            | otherwise = False

answer = head $ head $ filter fourConsec (map (\n -> drop n pfactors) [1..])
