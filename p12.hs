module Main where

import Data.List (nub, sort)

triangles = 1 : (zipWith (+) [2..] triangles)

ld n = ldf 2 n

divides d n = rem n d == 0

ldf k n | k `divides` n = k
        | k^2 > n       = n
        | otherwise     = ldf (k+1) n

factors n | n < 1     = error "Argument not positive"
          | n == 1    = []
          | otherwise = p : factors (div n p)
                        where p = ld n

uniqfactors n = 1 : (sort $ nub $ map product $ sublists (factors n))

sublists [] = []
sublists (x:xs) = [x] : ((map (x:) (sublists xs)) ++ (sublists xs))

trifacts = map (length . uniqfactors) triangles

main = print (triangles !! index)
    where index = length $ takeWhile (500 >) trifacts
