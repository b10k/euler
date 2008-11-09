module P193 where

import ONeillPrimes

squarefree = [1, 2, 3, 5] ++ (diff [6, 7 ..] squares) 
squares    = foldr1 f . map g $ (map (^2) primes)
  where 
    f (x:xt) ys = x : (merge xt ys)
    g p         = iterate (+p) p

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : (merge xt ys)
    EQ -> x : (merge xt yt)
    GT -> y : (merge xs yt)

diff :: (Ord a) => [a] -> [a] -> [a]
diff xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : (diff xt ys)
    EQ -> diff xt yt
    GT -> diff xs yt
