module P18 where

import System.IO

parseTri x = pt x 1
  where
    pt [] _ = []
    pt x n = let (a,b) = splitAt n x in a : (pt b (n+1))

f xs ys = zipWith3 g xs ys (tail ys)
g x y z = x + max y z
answer tri = foldr1 f tri

main = do
  f <- readFile "triangle.txt"
  let a = map read $ words f
  let b = parseTri a
  print $ answer b
