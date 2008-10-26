module P23 where

import Euler
import Data.List (delete, sort, nub)
import Data.Array

properSum n = sum $ delete n $ divisors n

abundant n = n < properSum n

aar = listArray (1,28123) $ map abundant [1..28123]

isAbundant = (aar !)

abundants = filter isAbundant [1..28123]

rests n = map (n-) $ takeWhile (<= n `div` 2) abundants

isSum = any isAbundant . rests
