module P45 where

triangles   = 1 : (zipWith (+) [2..] triangles)
pentagonals = 1 : (zipWith (+) [4,7..] pentagonals)
hexagonals  = 1 : (zipWith (+) [5,9..] hexagonals)

-- Intersection of sorted infinite lists
inter xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> inter xt ys
    EQ -> x : (inter xt yt)
    GT -> inter xs yt

