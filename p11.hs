module Main where

import Data.Array
import Data.List (maximum)
import System.IO

ph g (x,y) = product $ map (\n -> g ! (x,y+n)) [0..3]
pv g (x,y) = product $ map (\n -> g ! (x+n,y)) [0..3]
pd g (x,y) = product $ map (\n -> g ! (x+n,y+n)) [0..3]
pd2 g (x,y) = product $ map (\n ->g ! (x+n, y-n)) [0..3]

products g = 
  map (ph g) [(x,y) | x <- [1..20], y <- [1..17]]
  ++
  map (pv g) [(x,y) | x <- [1..17], y <- [1..20]] 
  ++
  map (pd g) [(x,y) | x <- [1..17], y <- [1..17]]
  ++
  map (pd2 g) [(x,y) | x <- [1..17], y <- [4..20]]

main = do
  f <- readFile "p11.txt"
  let grid = listArray ((1,1),(20,20)) (map read (words f) :: [Integer])
  print $ maximum (products grid)

