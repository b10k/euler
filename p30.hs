module Main where

import Numeric

digits n = reverse (d' n)
  where d' 0 = []
        d' n = let (x,y) = n `divMod` 10 in y : d' x

powFive = sum . (map (\x -> x^5)) . digits

main = return (sum [x | x <- [2..200000], x == powFive x])
